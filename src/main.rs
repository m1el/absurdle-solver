#![feature(iter_intersperse)]
#![feature(bool_to_option)]

use core::cmp::{Ord, PartialOrd, Reverse};
use core::sync::atomic::{AtomicUsize, Ordering as AtomicOrdering};
use std::collections::{BTreeMap};

mod words;

pub(crate) type Word = [u8; 5];

/*
const ALL_WORDS: usize = 2315;

type BitsetWord = u64;
const WORD_SIZE: usize = core::mem::size_of::<BitsetWord>() * 8;
const BITSET_LENGTH: usize = (ALL_WORDS + WORD_SIZE - 1) / WORD_SIZE;

struct WordSet {
    bitset: [BitsetWord; BITSET_LENGTH],
}

impl WordSet {
    fn all() -> Self {
        let mut result = Self {
            bitset: [!0; BITSET_LENGTH],
        };
        if let Some(last) = result.bitset.last_mut() {
            *last >>= BITSET_LENGTH * WORD_SIZE - ALL_WORDS;
        }
        result
    }

    fn size(&self) -> usize {
        self.bitset
            .iter()
            .map(|item| item.count_ones() as usize)
            .sum::<usize>()
    }

    fn iter<'a>(&'a self) -> impl Iterator<Item=Word> + 'a {
        self.bitset.iter()
            .copied().enumerate()
            .filter(|&(_index, bitset)| bitset != 0)
            .flat_map(|(index, bitset)| {
                println!("bs={:016x}", bitset);
                let start = index * WORD_SIZE;
                (0..WORD_SIZE).filter_map(move |bit| {
                    ((bitset >> bit) & 1 != 0)
                        .then(|| words::POSSIBLE_WORDS[bit + start])
                })
            })
    }
}
*/

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

type Buckets = BTreeMap<WordScore, Vec<Word>>;
fn get_rigged_response(
    buckets: &mut Buckets,
    secret_words: &mut Vec<Word>,
    guess: Word,
) -> WordScore {
    assert!(!secret_words.is_empty(), "The list of secret words cannot be empty");

    for &word in secret_words.iter() {
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

    let worst_bucket = buckets.get_mut(&max_key)
        .expect("The key comes from the hashmap, so it must be present");
    core::mem::swap(secret_words, worst_bucket);

    for bucket in buckets.values_mut() {
        bucket.clear();
    }

    max_key
}

fn descend_path(
    buckets: &mut Buckets,
    path: &mut Vec<Word>,
    remaining: &mut Vec<Word>,
    guess: Word,
) {
    path.push(guess);
    get_rigged_response(buckets, remaining, guess);
    if path.len() < 3 {
        for &word in words::POSSIBLE_WORDS {
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
            eprintln!("processed: {} path={} remaining={}", count, response, remaining.len());
        }
    }
    path.pop();
}

fn worker() {
    const ALL_WORDS: usize = words::POSSIBLE_WORDS.len(); // + words::IMPOSSIBLE_WORDS.len();
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

        let mut buckets = BTreeMap::new();
        let mut path = vec![];
        let mut remaining = words::POSSIBLE_WORDS.to_vec();
        for &guess in path.iter() {
            get_rigged_response(&mut buckets, &mut remaining, guess);
        }
        descend_path(&mut buckets, &mut path, &mut remaining, starting);
        let starting_str = core::str::from_utf8(&starting[..]).unwrap();
        // eprintln!("explored {}", starting_str);
    }
}

fn main() {
    //for word in WordSet::all().iter() {
    //    let word_str = std::str::from_utf8(&word[..]).unwrap();
    //    println!("{}", word_str);
    //}
    let max_threads = std::thread::available_parallelism()
        .expect("no paralllelism?").get();
    let mut threads = Vec::new();
    for _ in 0..max_threads {
        threads.push(std::thread::spawn(worker));
    }
    for thread in threads {
        thread.join().expect("some threads have crashed");
    }
}
