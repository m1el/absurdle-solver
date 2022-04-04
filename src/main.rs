use core::sync::atomic::{AtomicUsize, Ordering as AtomicOrdering};
use std::collections::{BTreeMap, BTreeSet};

mod game;
use game::{get_rigged_response, Buckets, Game, Word, WordScore, WORD_SIZE};

mod words;

/// The requested solution length. We're looking for 4-word Absurdle solutions.
const EXPECTED_PATH_LEN: usize = 4;

/// Convert solution path to a comma-separated string
fn path_to_string(path: &[Word]) -> String {
    /// Pre-calculated the string representation length.
    const EXPECTED_ROW_LEN: usize = EXPECTED_PATH_LEN * (WORD_SIZE + 1) - 1;
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

struct DepthSearch {
    /// Re-usable storage for `get_rigged_response`
    buckets: Buckets,
    /// Current list of remaining words for the recursive descent
    remaining: Vec<Word>,
    /// Current search path
    path: Vec<Word>,
    /// Prune size at depth 2
    prune: usize,
}

impl DepthSearch {
    /// Construct new depth search, with given parameters.
    /// This function will take care of reducing the word list according to
    /// the provided path.
    pub fn new(word_list: &[Word], path: Vec<Word>, prune: usize) -> Self {
        let mut remaining = word_list.to_vec();
        let mut buckets = Buckets::new();
        for &guess in path.iter() {
            get_rigged_response(&mut buckets, &mut remaining, guess);
        }
        Self {
            buckets,
            path,
            remaining,
            prune,
        }
    }

    /// Recursively run depth first search, and call `sink` with solutions.
    pub fn descend_path(&mut self, sink: &impl Fn(&[Word])) {
        /// Check pruning length at depth 2.  Pruning assumes that there's
        /// no way to reduce the remaining words list to a single word
        /// using only one guess.
        const PRUNE_DEPTH: usize = 2;

        /// Since the last word in the path needs to be known for the solution
        /// to be valid, the search depth is one less than the solution length.
        const MAX_SEARCH_DEPTH: usize = EXPECTED_PATH_LEN - 1;

        /// Global counter for pruned words. We're using this for stats.
        static PRUNED: AtomicUsize = AtomicUsize::new(0);

        let all_words = words::POSSIBLE_WORDS.iter().chain(words::IMPOSSIBLE_WORDS);
        if self.path.len() == PRUNE_DEPTH && self.remaining.len() > self.prune {
            // Hoping for the codegen to replace this with a constant
            let word_count = all_words.count();

            assert!(MAX_SEARCH_DEPTH == PRUNE_DEPTH + 1,
                "Requirement for calculating combinations for pruned words.");
            // Since there are 2 words in the path, there are 2 fewer words
            // to be explored than the total number of words.
            PRUNED.fetch_add(word_count - PRUNE_DEPTH, AtomicOrdering::Relaxed);
            return;
        }

        assert!(self.path.len() <= MAX_SEARCH_DEPTH,
            "Rath length should not exceed MAX_SEARCH_DEPTH");
        if self.path.len() == MAX_SEARCH_DEPTH {
            // Check that there is only one remaining word left, and call
            // the sink with the solution path.
            if self.remaining.len() == 1 {
                self.path.push(self.remaining[0]);
                sink(&self.path);
                self.path.pop();
            }

            // Global counter of explored paths.
            static COUNTER: AtomicUsize = AtomicUsize::new(0);
            let count = COUNTER.fetch_add(1, AtomicOrdering::Relaxed);
            // Print stats every ~million explored paths.
            if count & 0xfffff == 0 {
                let pruned = PRUNED.load(AtomicOrdering::Relaxed);
                eprintln!(
                    "processed={} pruned={} last_path={} remaining={}",
                    count,
                    pruned,
                    path_to_string(&self.path),
                    self.remaining.len()
                );
            }
        // If we haven't reached the max search depth, descend some more.
        } else {
            // Save the remaining words since a recursive call will mutate it
            let remaining_backup = self.remaining.clone();
            for &word in all_words {
                // There is no information to be gained from a repeating guess.
                if self.path.contains(&word) {
                    continue;
                }

                // Calculate the next state for the descent.
                self.path.push(word);
                get_rigged_response(&mut self.buckets, &mut self.remaining, word);

                self.descend_path(sink);

                // Restore the state prior to the recursive call.
                self.remaining.clear();
                self.remaining.extend(&remaining_backup);
                self.path.pop();
            }
        }
    }
}

fn solution_worker(position: &AtomicUsize, prune: usize, start: Option<Word>) {
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
        // current search word.
        let path = start.into_iter().chain(once(starting)).collect::<Vec<_>>();

        // Sink for the solutions.
        fn print_path(path: &[Word]) {
            println!("SOLUTION = {}", path_to_string(path));
        }
        // Run the depth first search, printing all the 4-word solutions
        DepthSearch::new(words::POSSIBLE_WORDS, path, prune)
            .descend_path(&print_path);
    }
}

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

        let mut buckets = Buckets::new();
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

fn solution_distribution() {
    use std::io::BufRead;

    let mut path_counts = BTreeMap::new();
    let mut uniq_counts = BTreeMap::new();
    let mut buckets = Buckets::new();

    let stdin = std::io::stdin();
    for line in stdin.lock().lines() {
        let line = line.unwrap();
        let words = line
            .trim()
            .split(',')
            .map(|w| w.as_bytes().try_into().expect("ivalid number of chars"));

        let mut remaining = words::POSSIBLE_WORDS.to_vec();
        for word in words.take(2) {
            get_rigged_response(&mut buckets, &mut remaining, word);
        }
        remaining.sort();

        *path_counts.entry(remaining.len()).or_insert(0) += 1;

        uniq_counts
            .entry(remaining.len())
            .or_insert_with(BTreeSet::new)
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
    use std::sync::{Arc, Mutex};
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
            let update_counts = distr_worker(&position);
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
    fn all_allowed(_word: Word) -> bool {
        true
    }
    let mut hard_mode = game::HardMode::new(vec![target], all_allowed);
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
        let words = line
            .trim()
            .split(',')
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
    use std::sync::Arc;
    let max_threads = std::thread::available_parallelism()
        .map(|x| x.get())
        .unwrap_or(1);

    eprintln!("Starting to find solutions with prune={}", prune);
    eprintln!("running with {} threads", max_threads);
    let mut threads = Vec::new();
    let position = Arc::new(AtomicUsize::new(0));
    for _ in 0..max_threads {
        let position = position.clone();
        threads.push(std::thread::spawn(move || {
            solution_worker(&position, prune, starting);
        }));
    }
    for thread in threads {
        thread.join().expect("some threads have crashed");
    }
}

fn highlight_term(score: WordScore, guess: Word) -> String {
    use game::LetterScore;
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
        if score.correct_places as usize == word.len() {
            println!("You won!");
            if !hard_mode && is_hard_solution(history) {
                println!("This also happens to be a hard mode solution.");
            }
            break;
        }
    }
}

fn print_help() {
    print!(
        r#"Invalid mode! Please provide a mode as the first command line argument. Available modes are:
- play [hard]: Play the game interactively in the console. Optionally add "hard" to play hard mode.
  Hard mode requires you to strictly use all the information previously acquired in the game.
- find-solutions [--prune <number>] [<starting word>]: Find 4-word solutions. Optionally provide with the starting word.
- two-word-distr: Calculate the distribution of remaining words for each pair of two starting words.
- solution-distr: Take solutions from stdin, print their distribution by the first two starting words.
- filter-hard: Take solutions from stdin, only print valid hard mode solutions.
- bench-guess-score: Benchmark how long it takes to calculate score for one word guess.
"#
    );
}

fn parse_solutions_args(
    mut argv: impl Iterator<Item = String>,
) -> Result<(usize, Option<Word>), &'static str> {
    const DEFAULT_PRUNE: usize = 40;
    let mut prune = None;
    let mut word = None;
    while let Some(arg) = argv.next() {
        if arg == "--prune" {
            if prune.is_some() {
                return Err("Duplicate argument for prune count");
            }
            let count = argv
                .next()
                .ok_or("expected an argument after --prune")?
                .parse::<usize>()
                .map_err(|_| "expected a number after --prune")?;
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
        "find-all-scores" => {
            use crate::game::guess_score;
            let mut scores = BTreeSet::new();
            let all_words = words::POSSIBLE_WORDS.iter().chain(words::IMPOSSIBLE_WORDS);
            for &first in all_words {
                for &second in words::POSSIBLE_WORDS {
                    scores.insert(guess_score(first, second).hash);
                }
            }
            println!("the set of all possible scores: {:?}", scores);
        }
        _ => print_help(),
    };
}
