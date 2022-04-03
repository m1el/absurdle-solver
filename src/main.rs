#![feature(iter_intersperse)]
//#![feature(bool_to_option)]

use core::sync::atomic::{AtomicUsize, Ordering as AtomicOrdering};

use game::{Buckets, GameMode, Word, get_rigged_response};

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

fn distr_worker() -> std::collections::BTreeMap<usize, usize> {
    const ALL_WORDS: usize = words::POSSIBLE_WORDS.len() + words::IMPOSSIBLE_WORDS.len();
    let mut counts = std::collections::BTreeMap::new();
    loop {
        static POSITION: AtomicUsize = AtomicUsize::new(0);
        let position = POSITION.fetch_add(1, AtomicOrdering::SeqCst);
        let first = if position < words::POSSIBLE_WORDS.len() {
            words::POSSIBLE_WORDS[position]
        } else if position < ALL_WORDS {
            let position = position - words::POSSIBLE_WORDS.len();
            words::IMPOSSIBLE_WORDS[position]
        } else {
            return counts;
        };

        let mut buckets = Buckets::new();
        let mut remaining = words::POSSIBLE_WORDS.to_vec();
        get_rigged_response(&mut buckets, &mut remaining, first);

        for &second in words::POSSIBLE_WORDS.iter().chain(words::IMPOSSIBLE_WORDS) {
            let mut remaining = remaining.clone();
            get_rigged_response(&mut buckets, &mut remaining, second);
            *counts.entry(remaining.len()).or_insert(0) += 1;
        }

        if position & 0xff == 0 {
            eprintln!("{} / {}", position, ALL_WORDS);
        }
    }
}

fn main() {
    let mode = "play";
    match mode {
        "sol-distr" => {
            use std::collections::{BTreeMap, HashSet};
            use std::io::BufRead;

            let mut path_counts = BTreeMap::new();
            let mut uniq_counts = BTreeMap::new();
            let mut buckets = Buckets::new();

            let stdin = std::io::stdin();
            for line in stdin.lock().lines() {
                let line = line.unwrap();
                let words = line.trim().split(',')
                    .map(|w| w.as_bytes().try_into().expect("ivalid number of chars"))
                    .collect::<Vec<Word>>();

                let mut remaining = words::POSSIBLE_WORDS.to_vec();
                for word in words.into_iter().take(2) {
                    get_rigged_response(&mut buckets, &mut remaining, word);
                }
                *path_counts.entry(remaining.len()).or_insert(0) += 1;
                uniq_counts.entry(remaining.len()).or_insert(HashSet::new())
                    .insert(remaining);
            }
            for (count, distr) in path_counts {
                println!("{} {}", count, distr);
            }
            println!("----");
            for (count, uniq) in uniq_counts {
                println!("{} {}", count, uniq.len());
            }
        }
        "distribution" => {
            use std::collections::BTreeMap;
            use std::sync::{Arc, Mutex};
            let counts = Arc::new(Mutex::new(BTreeMap::new()));
            let max_threads = std::thread::available_parallelism()
                .map(|x| x.get()).unwrap_or(1);

            let mut threads = Vec::new();
            for _ in 0..max_threads {
                let counts = counts.clone();
                threads.push(std::thread::spawn(move || {
                    let update_counts = distr_worker();
                    let mut counts = counts.lock().expect("cannot lock mutex");
                    for (key, distr) in update_counts {
                        *counts.entry(key).or_insert(0) += distr;
                    }
                }));
            }
            for thread in threads {
                thread.join().expect("some threads have crashed");
            }
            let counts = counts.lock().expect("cannot lock mutex");
            for (rem, count) in counts.iter() {
                println!("{} {}", rem, count);
            }
        }
        "filter-hard" => {
            use std::io::BufRead;
            let stdin = std::io::stdin();
            'outer: for line in stdin.lock().lines() {
                let line = line.unwrap();
                let mut words = line.trim().split(',')
                    .map(|w| w.as_bytes().try_into().expect("ivalid number of chars"))
                    .collect::<Vec<Word>>();
                let target = words.pop().expect("at least one word is expected");
                fn all_allowed(_word: Word) -> bool { true }
                let mut hard_mode = GameMode::new(vec![target], all_allowed, true);
                for word in words {
                    if hard_mode.update(word).is_err() {
                        continue 'outer;
                    }
                }
                println!("{}", line.trim());
            }
        }
        "bench_guess_score" => {
            let start = std::time::Instant::now();
            let mut void = unsafe { core::mem::zeroed() };
            for i in 0..1_000_000_000 {
                use game::guess_score;
                let a = words::IMPOSSIBLE_WORDS[i & 0xfff];
                let b = words::IMPOSSIBLE_WORDS[i & 0x1fff];
                let score = guess_score(a, b);
                unsafe {
                    core::ptr::write_volatile(&mut void, score);
                }
            }
            println!("1M guess_score: {:?}", start.elapsed());
        }
        "play" => {
            use std::io::{Write, BufRead};
            fn valid_guess(word: Word) -> bool {
                words::POSSIBLE_WORDS.contains(&word)
                    || words::IMPOSSIBLE_WORDS.contains(&word)
            }
            let stdin = std::io::stdin();
            let stdout = std::io::stdout();
            let mut buf = String::new();
            let mut read_word = move || -> Result<[u8; 5], Box<dyn std::error::Error>> {
                let mut stdout = stdout.lock();
                stdout.write(b"enter your guess: ")?;
                stdout.flush()?;
                buf.clear();
                stdin.lock().read_line(&mut buf)?;
                let mut word: Word = buf.trim().as_bytes().try_into()?;
                for chr in word.iter_mut() {
                    if !chr.is_ascii_alphabetic() {
                        return Err("input contains non-letter".into());
                    }
                    *chr = chr.to_ascii_uppercase();
                }
                Ok(word)
            };
            let hard_mode = false;
            let mut game = GameMode::new(words::POSSIBLE_WORDS.to_vec(), valid_guess, hard_mode);
            loop {
                let word = loop {
                    match read_word() {
                        Ok(word) => {
                            break word;
                        }
                        Err(err) => {
                            eprintln!("could not parse line: {:?}", err);
                            continue;
                        }
                    }
                };
                let score = match game.update(word) {
                    Ok(score) => score,
                    Err(err) => {
                        println!("Invalid guess: {}", err);
                        continue;
                    }
                };
                println!("{}", score.highlight_term(word));
                if score.correct_places == word.len() {
                    println!("you won!");
                    break;
                }
            }
        }
        "main" => {
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
        _ => {
            println!("invalid mode");
         }
    };
}
