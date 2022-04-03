use core::sync::atomic::{AtomicUsize, Ordering as AtomicOrdering};

use game::{Buckets, Game, WordScore, LetterScore, HardMode, Word, get_rigged_response};

mod game;
mod words;

fn path_to_string(path: &[Word]) -> String {
    let mut response = String::with_capacity(32);
    for chunk in path {
        let chunk_str = std::str::from_utf8(&chunk[..]).unwrap();
        if response.len() > 1 {
            response.push(',');
        }
        response.push_str(chunk_str);
    }
    response
}

fn descend_path(
    buckets: &mut Buckets,
    path: &mut Vec<Word>,
    remaining: &[Word],
    sink: &impl Fn(&[Word]),
    prune: usize,
) {
    static PRUNED: AtomicUsize = AtomicUsize::new(0);
    let all_words = words::POSSIBLE_WORDS.iter().chain(words::IMPOSSIBLE_WORDS);
    if path.len() == 2 && remaining.len() > prune {
        let word_count = all_words.count();
        PRUNED.fetch_add(word_count - 2, AtomicOrdering::Relaxed);
        return;
    }
    if path.len() < 3 {
        for &word in all_words {
            if path.contains(&word) { continue; }
            let mut remaining = remaining.to_vec();
            path.push(word);
            get_rigged_response(buckets, &mut remaining, word);
            descend_path(buckets, path, &remaining, sink, prune);
            path.pop();
        }
    } else {
        if remaining.len() == 1 {
            path.push(remaining[0]);
            sink(path);
            path.pop();
        }
        static COUNTER: AtomicUsize = AtomicUsize::new(0);
        let count = COUNTER.fetch_add(1, AtomicOrdering::Relaxed);
        if count & 0xfffff == 0 {
            let pruned = PRUNED.load(AtomicOrdering::Relaxed);
            eprintln!("processed={} pruned={} last_path={} remaining={}",
                      count, pruned, path_to_string(path), remaining.len());
        }
    }
}

fn solution_worker(prune: usize, start: Option<Word>) {
    let all_words = words::POSSIBLE_WORDS.iter().chain(words::IMPOSSIBLE_WORDS);
    loop {
        static POSITION: AtomicUsize = AtomicUsize::new(0);
        let position = POSITION.fetch_add(1, AtomicOrdering::SeqCst);
        let starting = match all_words.clone().nth(position) {
            Some(&word) => word,
            None => break,
        };

        let mut buckets = Buckets::new();
        let mut path = Vec::new();
        if let Some(start) = start {
            path.push(start);
        }
        path.push(starting);

        let mut remaining = words::POSSIBLE_WORDS.to_vec();
        for &guess in path.iter() {
            get_rigged_response(&mut buckets, &mut remaining, guess);
        }

        fn print_path(path: &[Word]) {
            println!("SOLUTION = {}", path_to_string(path));
        }
        descend_path(&mut buckets, &mut path, &remaining, &print_path, prune);
        // let starting_str = core::str::from_utf8(&starting[..]).unwrap();
        // eprintln!("explored {}", starting_str);
    }
}

fn distr_worker() -> std::collections::BTreeMap<usize, usize> {
    let all_words = words::POSSIBLE_WORDS.iter().chain(words::IMPOSSIBLE_WORDS);
    let count = all_words.clone().count();
    let mut counts = std::collections::BTreeMap::new();
    loop {
        static POSITION: AtomicUsize = AtomicUsize::new(0);
        let position = POSITION.fetch_add(1, AtomicOrdering::SeqCst);
        let first = match all_words.clone().nth(position) {
            Some(&word) => word,
            None => return counts,
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
            eprintln!("{} / {}", position, count);
        }
    }
}

fn solution_distribution() {
    use std::collections::{BTreeMap, BTreeSet};
    use std::io::BufRead;

    let mut path_counts = BTreeMap::new();
    let mut uniq_counts = BTreeMap::new();
    let mut buckets = Buckets::new();

    let stdin = std::io::stdin();
    for line in stdin.lock().lines() {
        let line = line.unwrap();
        let words = line.trim().split(',')
            .map(|w| w.as_bytes().try_into().expect("ivalid number of chars"));

        let mut remaining = words::POSSIBLE_WORDS.to_vec();
        for word in words.take(2) {
            get_rigged_response(&mut buckets, &mut remaining, word);
        }
        *path_counts.entry(remaining.len()).or_insert(0) += 1;
        uniq_counts.entry(remaining.len()).or_insert_with(BTreeSet::new)
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

fn calculate_two_word_distribution() {
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

fn is_hard_solution(mut words: Vec<Word>) -> bool {
    let target = words.pop().expect("at least one word is expected");
    fn all_allowed(_word: Word) -> bool { true }
    let mut hard_mode = HardMode::new(vec![target], all_allowed);
    for word in words {
        if hard_mode.update(word).is_err() {
            return false;
        }
    }
    true
}

fn parse_word(s: &str) -> Result<Word, &'static str> {
    let mut word: Word = match s.trim().as_bytes().try_into() {
        Ok(word) => word,
        Err(_) => return Err("Word length incorrect"),
    };
    for chr in word.iter_mut() {
        if !chr.is_ascii_alphabetic() {
            return Err("Word contains non-letter");
        }
        *chr = chr.to_ascii_uppercase();
    }
    Ok(word)
}

fn filter_hard_solutions() {
    use std::io::BufRead;
    let stdin = std::io::stdin();
    for line in stdin.lock().lines() {
        let line = line.unwrap();
        let words = line.trim().split(',')
            .map(parse_word)
            .collect::<Result<Vec<Word>, &str>>();
        let words = match words {
            Ok(words) => words,
            Err(err) => {
                eprintln!("invalid input: {}", err);
                continue;
            }
        };
        if is_hard_solution(words) {
            println!("{}", line.trim());
        }
    }
}

fn benchmark_guess_score() {
    let start = std::time::Instant::now();
    let mut void = unsafe { core::mem::zeroed() };
    const ITERATIONS: usize = 1_000_000_000;
    for i in 0..ITERATIONS {
        use game::guess_score;
        let a = words::IMPOSSIBLE_WORDS[i & 0xfff];
        let b = words::IMPOSSIBLE_WORDS[i & 0x1fff];
        let score = guess_score(a, b);
        unsafe {
            core::ptr::write_volatile(&mut void, score);
        }
    }
    let elapsed = start.elapsed();
    let per_iter = elapsed / (ITERATIONS as u32);
    println!("Time to calculate {} scores: {:?}", ITERATIONS, elapsed);
    println!("Time per iteration: {:?}", per_iter);
}

fn find_solutions(prune: usize, starting: Option<Word>) {
    let max_threads = std::thread::available_parallelism()
        .map(|x| x.get()).unwrap_or(1);

    eprintln!("Starting to find solutions with prune={}", prune);
    eprintln!("running with {} threads", max_threads);
    let mut threads = Vec::new();
    for _ in 0..max_threads {
        threads.push(std::thread::spawn(move || {
            solution_worker(prune, starting);
        }));
    }
    for thread in threads {
        thread.join().expect("some threads have crashed");
    }
}

fn highlight_term(score: WordScore, guess: Word) -> String {
    const ESCAPE: &str = "\x1b[";
    let mut result = String::new();
    for (&score, chr) in score.letters.iter().zip(guess) {
        let color = match score {
            LetterScore::Miss => "0m",
            LetterScore::CorrectLetter => "38;2;255;255;0m",
            LetterScore::CorrectPlace => "38;2;0;255;0m",
        };
        result.push_str(ESCAPE);
        result.push_str(color);
        result.push(chr as char);
    }
    result.push_str(ESCAPE);
    result.push_str("0m");
    result
}

fn play(hard_mode: bool) {
    use std::io::{Write, BufRead};
    fn valid_guess(word: Word) -> bool {
        words::POSSIBLE_WORDS.contains(&word)
            || words::IMPOSSIBLE_WORDS.contains(&word)
    }
    let stdin = std::io::stdin();
    let stdout = std::io::stdout();
    let mut history = Vec::new();
    let mut buf = String::new();
    let mut read_word = move || -> Result<[u8; 5], Box<dyn std::error::Error>> {
        let mut stdout = stdout.lock();
        stdout.write_all(b"enter your guess: ")?;
        stdout.flush()?;
        buf.clear();
        stdin.lock().read_line(&mut buf)?;
        let word = parse_word(&buf)?;
        Ok(word)
    };
    let mut game = Game::new(words::POSSIBLE_WORDS.to_vec(), valid_guess, hard_mode);
    loop {
        let word = match read_word() {
            Ok(word) => word,
            Err(err) => {
                eprintln!("could not parse line: {:?}", err);
                continue;
            }
        };
        history.push(word);
        let score = match game.update(word) {
            Ok(score) => score,
            Err(err) => {
                println!("Invalid guess: {}", err);
                continue;
            }
        };
        println!("{}", highlight_term(score, word));
        if score.correct_places == word.len() {
            println!("You won!");
            if !hard_mode && is_hard_solution(history) {
                println!("This also happens to be a hard mode solution.");
            }
            break;
        }
    }
}

fn print_help() {
    print!(r#"Invalid mode! Please provide a mode as the first command line argument. Available modes are:
- play [hard]: Play the game interactively in the console. Optionally add "hard" to play hard mode.
  Hard mode requires you to strictly use all the information previously acquired in the game.
- find-solutions [--prune <number>] [<starting word>]: Find 4-word solutions. Optionally provide with the starting word.
- two-word-distr: Calculate the distribution of remaining words for each pair of two starting words.
- solution-distr: Take solutions from stdin, print their distribution by the first two starting words.
- filter-hard: Take solutions from stdin, only print valid hard mode solutions.
- bench-guess-score: Benchmark how long it takes to calculate score for one word guess.
"#);
}

fn parse_solutions_args(mut argv: impl Iterator<Item=String>)
    -> Result<(usize, Option<Word>), &'static str>
{
    const DEFAULT_PRUNE: usize = 40;
    let mut prune = None;
    let mut word = None;
    while let Some(arg) = argv.next() {
        if arg == "--prune" {
            if prune.is_some() {
                return Err("Duplicate argument for prune count");
            }
            let count = argv.next().ok_or("expected an argument after --prune")?
                .parse::<usize>().map_err(|_| "expected a number after --prune")?;
            prune = Some(count);
        } else {
            if word.is_some() {
                return Err("Duplicate argument for the starting word");
            }
            word = Some(parse_word(&arg)?);
        }
    }

    Ok((prune.unwrap_or(DEFAULT_PRUNE), word))
}

fn main() {
    let mut argv = std::env::args().skip(1);
    let mode = argv.next().unwrap_or_default();
    match mode.as_str() {
        "solution-distr" => solution_distribution(),
        "two-word-distr" => calculate_two_word_distribution(),
        "filter-hard" => filter_hard_solutions(),
        "bench-guess-score" => benchmark_guess_score(),
        "play" => {
            let hard_mode = argv.next().map_or(false, |s| s == "hard");
            play(hard_mode);
        }
        "find-solutions" => {
            let (prune, starting) = match parse_solutions_args(argv) {
                Ok(v) => v,
                Err(err) => {
                    eprintln!("Could not parse arguments: {}", err);
                    return;
                }
            };
            find_solutions(prune, starting);
        }
        _ => print_help(),
    };
}
