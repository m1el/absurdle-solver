#![feature(iter_intersperse)]
#![feature(bool_to_option)]

use core::sync::atomic::{AtomicUsize, Ordering as AtomicOrdering};

use game::{Buckets, Word, get_rigged_response};

mod game;
mod words;

fn descend_path(
    buckets: &mut Buckets,
    path: &mut Vec<Word>,
    remaining: &mut Vec<Word>,
    guess: Word,
) {
    path.push(guess);
    get_rigged_response(buckets, remaining, guess);

    const DEPTH_2_PRUNE_SIZE: usize = 2315;
    static PRUNED: AtomicUsize = AtomicUsize::new(0);
    if path.len() == 2 && remaining.len() > DEPTH_2_PRUNE_SIZE {
        PRUNED.fetch_add(words::POSSIBLE_WORDS.len() - 2, AtomicOrdering::Relaxed);
        path.pop();
        return;
    }
    if path.len() < 3 {
        for &word in words::POSSIBLE_WORDS.iter().chain(words::IMPOSSIBLE_WORDS) {
            if path.contains(&word) { continue; }
            let mut remaining = remaining.clone();
            descend_path(buckets, path, &mut remaining, word);
        }
    } else {
        if remaining.len() == 1 {
            let mut response = String::with_capacity(32);
            for chunk in path.iter().chain(remaining.iter()) {
                let chunk_str = std::str::from_utf8(&chunk[..]).unwrap();
                if response.len() > 1 {
                    response.push(',');
                }
                response.push_str(chunk_str);
            }
            println!("SOLUTION = {}", response);
        }
        let mut response = String::with_capacity(32);
        for chunk in path.iter() {
            let chunk_str = std::str::from_utf8(&chunk[..]).unwrap();
            if response.len() > 1 {
                response.push(',');
            }
            response.push_str(chunk_str);
        }

        static COUNTER: AtomicUsize = AtomicUsize::new(0);
        let count = COUNTER.fetch_add(1, AtomicOrdering::Relaxed);
        if count & 0xfffff == 0 {
            let pruned = PRUNED.load(AtomicOrdering::Relaxed);
            eprintln!("processed={} pruned={} path={} remaining={}",
                      count, pruned, response, remaining.len());
        }
    }
    path.pop();
}

fn worker(start: Option<Word>) {
    const ALL_WORDS: usize = words::POSSIBLE_WORDS.len() + words::IMPOSSIBLE_WORDS.len();
    loop {
        static POSITION: AtomicUsize = AtomicUsize::new(0);
        let position = POSITION.fetch_add(1, AtomicOrdering::SeqCst);
        let starting = if position < words::POSSIBLE_WORDS.len() {
            words::POSSIBLE_WORDS[position]
        } else if position < ALL_WORDS {
            let position = position - words::POSSIBLE_WORDS.len();
            words::IMPOSSIBLE_WORDS[position]
        } else {
            break;
        };

        let mut buckets = Buckets::new();
        let mut path = vec![];
        if let Some(start) = start {
            path.push(start);
        }

        let mut remaining = words::POSSIBLE_WORDS.to_vec();
        for &guess in path.iter() {
            get_rigged_response(&mut buckets, &mut remaining, guess);
        }
        descend_path(&mut buckets, &mut path, &mut remaining, starting);
        // let starting_str = core::str::from_utf8(&starting[..]).unwrap();
        // eprintln!("explored {}", starting_str);
    }
}

fn main() {
    let word: Option<Word> = std::env::args().nth(1)
        .and_then(|x| x.as_bytes().try_into().ok());

    let max_threads = std::thread::available_parallelism()
        .map(|x| x.get()).unwrap_or(1);

    eprintln!("running with {} threads", max_threads);
    let mut threads = Vec::new();
    for _ in 0..max_threads {
        threads.push(std::thread::spawn(move || {
            worker(word);
        }));
    }
    for thread in threads {
        thread.join().expect("some threads have crashed");
    }
}
