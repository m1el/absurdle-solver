use core::cmp::{Ord, PartialOrd, Reverse};
use std::collections::{BTreeMap};

pub(crate) type Word = [u8; 5];

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum LetterScore {
    Miss,
    CorrectLetter,
    CorrectPlace,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct WordScore {
    pub correct_places: usize,
    pub correct_letters: usize,
    pub letters: [LetterScore; 5],
}

/*
impl WordScore {
    pub fn highlight_term(&self, guess: Word) -> String {
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
*/

pub fn guess_score(mut secret: Word, guess: Word) -> WordScore {
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

pub type Buckets = BTreeMap<WordScore, Vec<Word>>;
pub fn get_rigged_response(
    buckets: &mut Buckets,
    secret_words: &mut Vec<Word>,
    guess: Word,
) -> WordScore {
    assert!(!secret_words.is_empty(), "The list of secret words cannot be empty");

    for bucket in buckets.values_mut() {
        bucket.clear();
    }

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

    max_key
}
