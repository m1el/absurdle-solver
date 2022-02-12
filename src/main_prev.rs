#![feature(iter_intersperse)]
use core::cmp::{Ord, PartialOrd, Ordering, Reverse};
use core::sync::atomic::{AtomicUsize, Ordering as AtomicOrdering};
use std::collections::{BinaryHeap, HashMap};

mod words;

pub(crate) type Word = [u8; 5];

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
enum LetterScore {
    Miss,
    CorrectLetter,
    CorrectPlace,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct WordScore {
    correct_places: usize,
    correct_letters: usize,
    letters: [LetterScore; 5],
}

impl WordScore {
    fn highlight_term(&self, guess: Word) -> String {
        const ESCAPE: &str = "\x1b[";
        let mut result = String::new();
        for (&score, chr) in self.letters.iter().zip(guess) {
            let color = match score {
                LetterScore::Miss => "0m",
                LetterScore::CorrectLetter => "33m",
                LetterScore::CorrectPlace => "32m",
            };
            result.push_str(ESCAPE);
            result.push_str(color);
            result.push(chr as char);
        }
        result.push_str(ESCAPE);
        result.push_str("0m");
        result
    }
}

fn guess_score(mut secret: Word, guess: Word) -> WordScore {
    let mut letters = [LetterScore::Miss; 5];

    // Prioritize finding letters that are in the correct place
    let mut correct_places = 0;
    for (ii, chr) in guess.into_iter().enumerate() {
        if secret[ii] == chr {
            // Mark letters as used in the secret word
            secret[ii] = 0;
            letters[ii] = LetterScore::CorrectPlace;
            correct_places += 1;
        }
    }

    let mut correct_letters = 0;
    for (ii, chr) in guess.into_iter().enumerate() {
        // Skip letters that were found to be in the right place
        if letters[ii] == LetterScore::CorrectPlace {
            continue;
        }

        if let Some(place) = secret.iter().position(|&s| s == chr) {
            // Mark letters as used in the secret word
            secret[place] = 0;
            letters[ii] = LetterScore::CorrectLetter;
            correct_letters += 1;
        }
    }

    WordScore {
        correct_letters,
        correct_places,
        letters,
    }
}

fn get_rigged_response(secret_words: &[Word], guess: Word)
    -> (WordScore, Vec<Word>)
{
    assert!(!secret_words.is_empty(), "The list of secret words cannot be empty");

    let mut buckets = HashMap::new();
    for &word in secret_words {
        let score = guess_score(word, guess);
        buckets.entry(score)
            .or_insert_with(Vec::new)
            .push(word);
    }

    let max_key =
        buckets.keys()
            // Find the bucket with maximum number of entries and minimal score
            .max_by_key(|&score| (buckets[score].len(), Reverse(score)))
            .expect("At least one key must be present, which means max must return Some")
            .clone();
    let words = buckets.remove(&max_key)
        .expect("The key comes from the hashmap, so it must be present");

    (max_key, words)
}

fn find_path(word: Word) -> Option<Vec<Word>> {
    struct GraphNode {
        path: Vec<Word>,
        words: Vec<Word>,
    }
    impl GraphNode {
        fn start() -> Self {
            Self {
                path: Vec::new(),
                words: words::POSSIBLE_WORDS.to_vec(),
            }
        }
        fn score(&self) -> Reverse<(usize, usize)> {
            Reverse((self.words.len(), self.path.len()))
        }
    }
    impl PartialEq for GraphNode {
        fn eq(&self, other: &GraphNode) -> bool {
            self.score() == other.score()
        }
    }
    impl Eq for GraphNode {}
    impl PartialOrd for GraphNode {
        fn partial_cmp(&self, other: &GraphNode) -> Option<Ordering> {
            Some(self.cmp(other))
        }
    }
    impl Ord for GraphNode {
        fn cmp(&self, other: &GraphNode) -> Ordering {
            self.score().cmp(&other.score())
        }
    }
    let mut min_words = words::POSSIBLE_WORDS.len();
    let mut queue = BinaryHeap::new();
    queue.push(GraphNode::start());
    while let Some(node) = queue.pop() {
        if node.words.len() < min_words {
            min_words = node.words.len();
            eprintln!("new minimum: {}", min_words);
        }
        if node.words.len() == 1 && node.words[0] == word {
            let mut path = node.path;
            path.push(word);
            return Some(path);
        }

        for &guess in words::POSSIBLE_WORDS.iter().chain(words::IMPOSSIBLE_WORDS.iter()) {
            if node.path.iter().any(|&p| p == guess) {
                continue;
            }
            let (_score, words) = get_rigged_response(&node.words, guess);
            // if score.correct_places != 0 || score.correct_letters != 0 {
            //     continue;
            // }
            if node.words.len() == words.len() {
                continue;
            }
            if !words.iter().any(|&w| w == word) {
                continue;
            }
            let mut path = node.path.clone();
            path.push(guess);
            queue.push(GraphNode { path, words });
        }
    }
    None
}

static POSITION: AtomicUsize = AtomicUsize::new(0);

fn worker() {
    loop {
        let position = POSITION.fetch_add(1, AtomicOrdering::SeqCst);
        if position >= words::POSSIBLE_WORDS.len() { break; }
        let needle = words::POSSIBLE_WORDS[position];
        let needle_str = core::str::from_utf8(&needle[..]).unwrap();
        eprintln!("looking for {}", needle_str);
        if let Some(path) = find_path(needle) {
            let mut result = String::new();
            for word in path {
                if !result.is_empty() {
                    result.push(',');
                }
                result.push_str(core::str::from_utf8(&word[..]).unwrap());
            }
            println!("{}: {}", needle_str, result);
        } else {
            println!("{}: ---", needle_str);
        }
    }
}

fn main() {
    const MAX_THREADS: usize = 8;
    let mut threads = Vec::new();
    for _ in 0..MAX_THREADS {
        threads.push(std::thread::spawn(worker));
    }
    for thread in threads {
        thread.join().expect("some threads have crashed");
    }

    let guesses = &[
        *b"AIERY",
        *b"CANST",
        *b"DOLMA",
        *b"PUPAL",
    ];
    let mut secret_words = words::POSSIBLE_WORDS.to_vec();
    for &guess in guesses {
        let guess_str = core::str::from_utf8(&guess[..]).unwrap();
        let (score, words) = get_rigged_response(&secret_words, guess);
        println!("You guessed: {}, words left: {}",
            score.highlight_term(guess), words.len());
        if score.correct_places == 5 {
            println!("Your guess {} is correct, you won!", guess_str);
            continue;
        }
        secret_words = words;
    }
}

