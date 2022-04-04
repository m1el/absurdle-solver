use core::cmp::{Ord, PartialOrd, Reverse};

pub const WORD_SIZE: usize = 5;
pub type Word = [u8; WORD_SIZE];
pub struct Buckets {
    keys: Vec<Option<WordScore>>,
    storage: Vec<Vec<Word>>,
}
impl Buckets {
    pub fn new() -> Self {
        Self {
            keys: vec![None; 256],
            storage: vec![Vec::new(); 256],
        }
    }
    fn clear(&mut self) {
        for key in self.keys.iter_mut() {
            *key = None;
        }
        for val in self.storage.iter_mut() {
            val.clear();
        }
    }
    fn get(&self, score: &WordScore) -> &Vec<Word> {
        let pos = (score.hash as usize) & 0xff;
        // assert!(self.keys[pos].is_some(), "missing key");
        // assert!(self.keys[pos] == Some(*score), "hash collision");
        &self.storage[pos]
    }
    fn get_mut(&mut self, score: &WordScore) -> &mut Vec<Word> {
        let pos = (score.hash as usize) & 0xff;
        match self.keys[pos] {
            Some(_key) => {}, // assert!(key == *score, "hash collision"),
            None => self.keys[pos] = Some(*score),
        }
        &mut self.storage[pos]
    }
}


#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum LetterScore {
    Miss = 0,
    CorrectLetter = 1,
    CorrectPlace = 2,
}

#[derive(Clone, Copy, Debug, PartialOrd, Ord)]
pub struct WordScore {
    pub correct_places: u8,
    pub correct_letters: u8,
    pub letters: [LetterScore; WORD_SIZE],
    pub hash: u8,
}

impl PartialEq for WordScore {
    fn eq(&self, other: &Self) -> bool {
        self.letters == other.letters
    }
}
impl Eq for WordScore { }

pub fn guess_score(mut secret: Word, guess: Word) -> WordScore {
    let mut letters = [LetterScore::Miss; WORD_SIZE];

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

    let hash = (letters[0] as u8) * 81 +
        (letters[1] as u8) * 27 +
        (letters[2] as u8) * 9 +
        (letters[3] as u8) * 3 +
        (letters[4] as u8) * 1;

    WordScore {
        correct_letters,
        correct_places,
        letters,
        hash,
    }
}

pub fn get_rigged_response(
    buckets: &mut Buckets,
    secret_words: &mut Vec<Word>,
    guess: Word,
) -> WordScore {
    assert!(
        !secret_words.is_empty(),
        "The list of secret words cannot be empty"
    );

    buckets.clear();

    for &word in secret_words.iter() {
        let score = guess_score(word, guess);
        let val = buckets.get_mut(&score);
        val.push(word);
    }

    let max_key = buckets.keys.iter().filter_map(|&x| x)
        // Find the bucket with maximum number of entries and minimal score
        .max_by_key(|&score| (buckets.get(&score).len(), Reverse(score)))
        .expect("At least one key must be present, which means max must return Some");

    let worst_bucket = buckets.get_mut(&max_key);
    core::mem::swap(secret_words, worst_bucket);

    max_key
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
            return Err("the word is not in the allowed list!".to_string());
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
                return Err(format!(
                    "the word must use char '{}' at position {}",
                    rc as char,
                    pos + 1
                ));
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
            return Err("the word is not in the allowed list!".to_string());
        }
        Ok(get_rigged_response(
            &mut self.buckets,
            &mut self.remaining,
            word,
        ))
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
