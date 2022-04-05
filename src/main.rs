use core::fmt;
use core::sync::atomic::{AtomicUsize, Ordering as AtomicOrdering};
use std::collections::{BTreeMap, BTreeSet};
use std::sync::Arc;

mod depth_search;
mod game;
mod words;

/// The requested solution length. We're looking for 4-word Absurdle solutions.
pub const EXPECTED_PATH_LEN: usize = 4;

/// Lazy way of combining all the errors into a single type.
type LazyResult<T> = Result<T, Box<dyn std::error::Error>>;

use crate::depth_search::{DepthSearch, DepthSearchStats};
use crate::game::*;

/// Parse command line options and run the corresponding sub-command.
/// The parsing could have been simplified by using a crate. However, that would
/// add external dependencies, which are to be avoided here.
fn main() {
    let mut argv = std::env::args().skip(1);
    let mode = argv.next().unwrap_or_default();
    match mode.as_str() {
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
        "two-word-distr" => calculate_two_word_distribution(),
        "solution-distr" => solution_distribution(),
        "filter-hard" => filter_hard_solutions(),
        "bench-guess-score" => benchmark_guess_score(),
        "find-all-scores" => find_all_scores(),
        _ => print_help(),
    };
}

/// Print possible subcommands and options
fn print_help() {
    print!(
        r#"Invalid mode! Please provide a mode as the first command line argument. Available modes are:
- play [hard]: Play the game interactively in the console.  Optionally add "hard" to play hard mode.
  Hard mode requires you to strictly use all the information previously acquired in the game.
- find-solutions [--prune <number>] [<starting word>]: Find 4-word solutions.  Optionally provide with the starting word.
- two-word-distr: Calculate the distribution of remaining pool sizes for all combinations of two starting words.
- solution-distr: Take solutions from stdin, print their distribution by the first two starting words.
- filter-hard: Take solutions from stdin, only print valid hard mode solutions.
- bench-guess-score: Benchmark how long it takes to calculate score for one word guess.
- find-all-scores: A test function to check whether the hash function is fitting for the purpose.
"#
    );
}

/// Play the game interactively in the console.
fn play(hard_mode: bool) {
    use std::io::{BufRead, Write};
    fn valid_guess(word: Word) -> bool {
        words::POSSIBLE_WORDS.contains(&word)
            || words::IMPOSSIBLE_WORDS.contains(&word)
    }
    let stdin = std::io::stdin();
    let stdout = std::io::stdout();
    let mut history = Vec::new();
    let mut buf = String::new();
    // a lambda that encapsulates reading the word from stdin
    let mut read_word = move || -> LazyResult<Word> {
        let mut stdout = stdout.lock();
        // Prompt the user to enter the guess.
        stdout.write_all(b"enter your guess: ")?;
        // Flushing is required since rust buffers stdout until newline.
        stdout.flush()?;
        buf.clear();
        stdin.lock().read_line(&mut buf)?;
        let word = parse_word(&buf)?;
        Ok(word)
    };

    let mut game = Game::new(words::POSSIBLE_WORDS.to_vec(), valid_guess, hard_mode);
    loop {
        // Try to read the word, or print an error if we've failed
        let word = match read_word() {
            Ok(word) => word,
            Err(err) => {
                eprintln!("could not parse line: {}", err);
                continue;
            }
        };
        history.push(word);

        // Try to run game logic or print an error.
        let score = match game.update(word) {
            Ok(score) => score,
            Err(err) => {
                println!("Invalid guess: {}", err);
                continue;
            }
        };

        // Print game's response to the player input.
        println!("{}", highlight_term(score, word));

        // Check win condition
        if score.is_winning() {
            println!("You won!");
            if !hard_mode && is_hard_solution(&history) {
                println!("This also happens to be a hard mode solution.");
            }
            break;
        }
    }
}

#[derive(Clone, Copy, Debug)]
enum ParseWordError {
    BadWordLength,
    BadChar,
}
impl fmt::Display for ParseWordError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            ParseWordError::BadWordLength => f.write_str("Word length incorrect"),
            ParseWordError::BadChar => f.write_str("Word contains a non-letter"),
        }
    }
}

impl std::error::Error for ParseWordError {}

/// Parse a word from a string.
fn parse_word(s: &str) -> Result<Word, ParseWordError> {
    let mut word: Word = match s.trim().as_bytes().try_into() {
        Ok(word) => word,
        Err(_) => return Err(ParseWordError::BadWordLength),
    };
    for chr in word.iter_mut() {
        if !chr.is_ascii_alphabetic() {
            return Err(ParseWordError::BadChar);
        }
        *chr = chr.to_ascii_uppercase();
    }
    Ok(word)
}

/// Use VT-codes to highlight user's guess according to the word score.
fn highlight_term(score: GuessScore, guess: Word) -> String {
    const ESCAPE: &str = "\x1b[";
    let mut result = String::new();
    let mut prev_score = None;
    for (&score, chr) in score.letters.iter().zip(guess) {
        if prev_score != Some(score) {
            let color = match score {
                LetterScore::Miss => "0m",
                LetterScore::CorrectLetter => "38;2;255;255;0m",
                LetterScore::CorrectPlace => "38;2;0;255;0m",
            };
            result.push_str(ESCAPE);
            result.push_str(color);
            prev_score = Some(score);
        }
        result.push(chr as char);
    }
    result.push_str(ESCAPE);
    result.push_str("0m");
    result
}

/// Parse command line arguments for `find-solutions` subcommand
/// The format is `[--prune <number>] [<starting word>]`, with both parts being
/// optional and non-repeating.
fn parse_solutions_args(
    mut argv: impl Iterator<Item = String>,
) -> LazyResult<(usize, Option<Word>)> {
    const DEFAULT_PRUNE: usize = 40;
    let mut prune = None;
    let mut word = None;
    while let Some(arg) = argv.next() {
        if arg == "--prune" {
            if prune.is_some() {
                return Err("Duplicate argument for prune count".into());
            }
            let count = argv
                .next()
                .ok_or("expected an argument after --prune")?
                .parse::<usize>()
                .map_err(|_| "expected a number after --prune")?;
            prune = Some(count);
        } else {
            if word.is_some() {
                return Err("Duplicate argument for the starting word".into());
            }
            word = Some(parse_word(&arg)?);
        }
    }

    Ok((prune.unwrap_or(DEFAULT_PRUNE), word))
}

/// Find four-word absurdle solutions.
/// `prune` configures pruning algorithm at step 2, which skips this branch
/// if there are too many words remaining.
/// `starting` optionally specifies the first word to use in each path.
fn find_solutions(prune: usize, starting: Option<Word>) {
    let max_threads = std::thread::available_parallelism()
        .map(|x| x.get())
        .unwrap_or(1);

    eprintln!("Starting to find solutions with prune={}", prune);
    eprintln!("running with {} threads", max_threads);
    let mut threads = Vec::new();
    let position = Arc::new(AtomicUsize::new(0));
    let stats = Arc::new(DepthSearchStats::new());

    // Create worker threads
    for _ in 0..max_threads {
        // Threads all get shared position and stats references
        let position = position.clone();
        let stats = stats.clone();
        stats.running.fetch_add(1, AtomicOrdering::Relaxed);
        threads.push(std::thread::spawn(move || {
            solution_worker(&*stats, &position, prune, starting);
        }));
    }

    // Create a thread to print statistics
    threads.push(std::thread::spawn(move || {
        use std::time::{Duration, Instant};
        let start = Instant::now();
        while stats.running.load(AtomicOrdering::Relaxed) != 0 {
            std::thread::sleep(Duration::from_millis(1000));
            let pruned = stats.pruned.load(AtomicOrdering::Relaxed);
            let processed = stats.processed.load(AtomicOrdering::Relaxed);
            let pps = (processed as f64) / start.elapsed().as_secs_f64();
            eprintln!(
                "processed={} pruned={} per_second={:.2}",
                processed, pruned, pps,
            );
        }
    }));
    for thread in threads {
        thread.join().expect("some threads have crashed");
    }
}

/// Worker function for finding the solutions.
fn solution_worker(
    stats: &DepthSearchStats,
    position: &AtomicUsize,
    prune: usize,
    start: Option<Word>,
) {
    use core::iter::once;
    let all_words = words::POSSIBLE_WORDS.iter().chain(words::IMPOSSIBLE_WORDS);
    loop {
        // Multi-thread iteration over all words.
        let position = position.fetch_add(1, AtomicOrdering::SeqCst);
        // Hoping for the codegen to compile `nth` to few branches.
        let starting = match all_words.clone().nth(position) {
            Some(&word) => word,
            None => break,
        };

        // Initialize the path with optionally provided starting word and
        // the current search word.
        let path = start.into_iter().chain(once(starting)).collect::<Vec<_>>();

        /// Sink for the solutions.
        fn print_path(path: &[Word]) {
            println!("SOLUTION = {}", path_to_string(path));
        }
        // Run depth first search, printing all 4-word solutions
        DepthSearch::new(words::POSSIBLE_WORDS, path, prune, stats)
            .descend_path(&print_path);
    }
    stats.running.fetch_sub(1, AtomicOrdering::Relaxed);
}

/// Convert solution path to a comma-separated string
fn path_to_string(path: &[Word]) -> String {
    /// Pre-calculated the string representation length.
    const EXPECTED_ROW_LEN: usize = EXPECTED_PATH_LEN * (WORD_LENGTH + 1) - 1;
    let mut response = String::with_capacity(EXPECTED_ROW_LEN);
    for chunk in path {
        let chunk_str = std::str::from_utf8(&chunk[..]).unwrap();
        if response.len() > 1 {
            response.push(',');
        }
        response.push_str(chunk_str);
    }
    response
}

/// Calculate the distribution of remaining secret words count for
/// all combinations of two starting words.
fn calculate_two_word_distribution() {
    use std::sync::Mutex;
    let counts = Arc::new(Mutex::new(BTreeMap::new()));
    let max_threads = std::thread::available_parallelism()
        .map(|x| x.get())
        .unwrap_or(1);

    let mut threads = Vec::new();
    let position = Arc::new(AtomicUsize::new(0));
    for _ in 0..max_threads {
        let counts = counts.clone();
        let position = position.clone();
        threads.push(std::thread::spawn(move || {
            // The worker function is expected to run for a long time
            // and return counters for partial distribution.
            let update_counts = distr_worker(&position);
            // ... then the counters are merged together.
            let mut counts = counts.lock().expect("cannot lock mutex");
            for (key, distr) in update_counts {
                *counts.entry(key).or_insert(0) += distr;
            }
        }));
    }
    for thread in threads {
        thread.join().expect("some threads have crashed");
    }

    // When the threads have done their job, all the counters should be merged.
    let counts = counts.lock().expect("cannot lock mutex");
    for (rem, count) in counts.iter() {
        println!("{} {}", rem, count);
    }
}

/// Thread worker function for calculating the distribution of remaining
/// secret words after 2 steps.
fn distr_worker(position: &AtomicUsize) -> BTreeMap<usize, usize> {
    let all_words = words::POSSIBLE_WORDS.iter().chain(words::IMPOSSIBLE_WORDS);
    // Hoping for the codegen to replace this with a constant
    let count = all_words.clone().count();
    let mut counts = BTreeMap::new();
    loop {
        // Multi-thread iteration over all words.
        let position = position.fetch_add(1, AtomicOrdering::SeqCst);
        // Hoping for the codegen to compile `nth` to few branches.
        let first = match all_words.clone().nth(position) {
            Some(&word) => word,
            None => return counts,
        };

        let mut buckets = DSHashMap::new();
        let mut remaining = words::POSSIBLE_WORDS.to_vec();
        get_rigged_response(&mut buckets, &mut remaining, first);

        let mut remaining_scratch = Vec::new();
        for &second in all_words.clone() {
            remaining_scratch.clear();
            remaining_scratch.extend(&remaining);
            get_rigged_response(&mut buckets, &mut remaining_scratch, second);
            *counts.entry(remaining_scratch.len()).or_insert(0) += 1;
        }

        if position & 0xff == 0 {
            eprintln!("{} / {}", position, count);
        }
    }
}

/// Read solutions from standard input and print their distribution
/// by first two starting words and by the remaining words.
fn solution_distribution() {
    use std::io::BufRead;

    let mut path_counts = BTreeMap::new();
    let mut uniq_counts = BTreeMap::new();
    let mut buckets = DSHashMap::new();

    let stdin = std::io::stdin();
    for line in stdin.lock().lines() {
        let line = line.unwrap();
        let words = line
            .split(',')
            .map(parse_word)
            .collect::<Result<Vec<Word>, ParseWordError>>();
        let words = match words {
            Ok(words) => words,
            Err(err) => {
                eprintln!("invalid input: {:?}", err);
                continue;
            }
        };
        if words.is_empty() {
            continue;
        }

        let mut remaining = words::POSSIBLE_WORDS.to_vec();
        for &word in &words[..2] {
            get_rigged_response(&mut buckets, &mut remaining, word);
        }
        remaining.sort_unstable();

        *path_counts.entry(remaining.len()).or_insert(0) += 1;

        uniq_counts
            .entry(remaining.len())
            .or_insert_with(BTreeSet::new)
            .insert(remaining);
    }

    println!("group size -> number of paths:");
    for (count, distr) in path_counts {
        println!("{} {}", count, distr);
    }

    println!("group size -> number of unique groups:");
    for (count, uniq) in uniq_counts {
        println!("{} {}", count, uniq.len());
    }
}

/// Read solutions from stdin, only print hard solutions to stdout.
/// Each solution is a line of comma-separated words.
fn filter_hard_solutions() {
    use std::io::BufRead;
    let stdin = std::io::stdin();
    for line in stdin.lock().lines() {
        let line = line.unwrap();
        let words = line
            .split(',')
            .map(parse_word)
            .collect::<Result<Vec<Word>, ParseWordError>>();
        let words = match words {
            Ok(words) => words,
            Err(err) => {
                eprintln!("invalid input: {}", err);
                continue;
            }
        };
        if words.is_empty() {
            continue;
        }
        if is_hard_solution(&words) {
            println!("{}", line.trim());
        }
    }
}

/// Benchmark `guess_score` function
fn benchmark_guess_score() {
    let start = std::time::Instant::now();
    // poor man's std::hint::black_box
    let mut void = unsafe { core::mem::zeroed() };
    const ITERATIONS: usize = 1_000_000_000;
    for i in 0..ITERATIONS {
        let a = words::IMPOSSIBLE_WORDS[i & 0xfff];
        let b = words::IMPOSSIBLE_WORDS[i & 0x1fff];
        let score = game::guess_score(a, b);
        // poor man's std::hint::black_box
        unsafe { core::ptr::write_volatile(&mut void, score) };
    }
    let elapsed = start.elapsed();
    let per_iter = elapsed / (ITERATIONS as u32);
    println!("Time to calculate {} scores: {:?}", ITERATIONS, elapsed);
    println!("Time per iteration: {:?}", per_iter);
}

/// Verify that the custom hash for the scores has no collisions.
fn find_all_scores() {
    let mut scores = BTreeSet::new();
    let all_words = words::POSSIBLE_WORDS.iter().chain(words::IMPOSSIBLE_WORDS);
    for &first in all_words {
        for &second in words::POSSIBLE_WORDS {
            scores.insert(guess_score(first, second));
        }
    }
    let hashes = scores.iter().map(|&x| x.hash).collect::<BTreeSet<_>>();
    println!(
        "the count of all scores: {} (should be {})",
        scores.len(),
        3_usize.pow(WORD_LENGTH as u32) - WORD_LENGTH
    );
    // println!("the set of all possible scores: {:?}", scores);
    println!(
        "the hash is a pefect hash for 256 elements: {}",
        hashes.len() == scores.len()
    );
}
