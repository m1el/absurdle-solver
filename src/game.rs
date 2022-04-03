use core::cmp::{Ord, PartialOrd, Reverse};
use std::collections::{BTreeMap};

pub type Word = [u8; 5];
pub type Buckets = BTreeMap<WordScore, Vec<Word>>;

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

impl WordScore {
    pub fn highlight_term(&self, guess: Word) -> String {
        const ESCAPE: &str = "\x1b[";
        let mut result = String::new();
        for (&score, chr) in self.letters.iter().zip(guess) {
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
}

pub type CheckAllowed = fn(word: Word) -> bool;

pub struct HardMode {
    buckets: Buckets,
    remaining: Vec<Word>,
    allowed: CheckAllowed,
    miss_chars: Vec<u8>,
    req_chars: Vec<u8>,
    req_place: Vec<(u8, usize)>,
}

impl HardMode {
    pub fn new(remaining: Vec<Word>, allowed: CheckAllowed) -> Self {
        Self {
            buckets: Buckets::new(),
            remaining,
            allowed,
            miss_chars: Vec::new(),
            req_chars: Vec::new(),
            req_place: Vec::new(),
        }
    }
    pub fn update(&mut self, word: Word) -> Result<WordScore, String> {
        if !(self.allowed)(word) {
            return Err(format!("the word is not in the allowed list!"));
        }
        let mut cword = word;
        for &mc in &self.miss_chars {
            if cword.contains(&mc) {
                return Err(format!("the word must not use char '{}'", mc as char));
            }
        }
        for &(rc, pos) in &self.req_place {
            if cword[pos] != rc {
                cword[pos] = 0;
                return Err(format!("the word must use char '{}' at position {}",
                    rc as char, pos + 1));
            }
        }
        for &rc in &self.req_chars {
            if let Some(pos) = cword.iter().position(|&c| c == rc) {
                cword[pos] = 0;
            } else {
                return Err(format!("the word must use char '{}'", rc as char));
            }
        }

        let score = get_rigged_response(&mut self.buckets, &mut self.remaining, word);
        self.req_chars.clear();
        self.req_place.clear();

        for (index, (chr, ls)) in word.iter().copied().zip(score.letters).enumerate() {
            match ls {
                LetterScore::Miss => {
                    self.miss_chars.push(chr);
                }
                LetterScore::CorrectLetter => {
                    self.req_chars.push(chr);
                }
                LetterScore::CorrectPlace => {
                    self.req_place.push((chr, index));
                }
            }
        }
        Ok(score)
    }
}

pub struct RegularMode {
    buckets: Buckets,
    remaining: Vec<Word>,
    allowed: CheckAllowed,
}

impl RegularMode {
    pub fn new(remaining: Vec<Word>, allowed: CheckAllowed) -> Self {
        Self {
            buckets: Buckets::new(),
            remaining,
            allowed,
        }
    }
    pub fn update(&mut self, word: Word) -> Result<WordScore, String> {
        if !(self.allowed)(word) {
            return Err(format!("the word is not in the allowed list!"));
        }
        Ok(get_rigged_response(&mut self.buckets, &mut self.remaining, word))
    }
}

pub enum Game {
    Regular(RegularMode),
    Hard(HardMode),
}

impl Game {
    pub fn new(remaining: Vec<Word>, allowed: CheckAllowed, hard: bool) -> Self {
        if hard {
            Game::Hard(HardMode::new(remaining, allowed))
        } else {
            Game::Regular(RegularMode::new(remaining, allowed))
        }

    }
    pub fn update(&mut self, word: Word) -> Result<WordScore, String> {
        match self {
            Game::Regular(regular) => regular.update(word),
            Game::Hard(hard) => hard.update(word),
        }
    }
}

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
