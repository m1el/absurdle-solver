use core::cmp::{Ord, PartialOrd, Reverse};
use core::fmt;

/// Word length.
pub const WORD_LENGTH: usize = 5;

/// Type alias used for a word.
/// We choose fixed length array representation since it's one of the most
/// compact representations. Using a &str or String would require 2 or more
/// pointer-sized values per word. Using bit-packed representation could
/// reduce this to 4 bytes or u32, but that would require additional processing.
pub type Word = [u8; WORD_LENGTH];

/// The score of an individual letter.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum LetterScore {
    /// This letter is not needed for the secret word
    Miss = 0,
    /// This is a correct letter, but it's located at a different place
    CorrectLetter = 1,
    /// This is a correct letter at the correct place
    CorrectPlace = 2,
}

/// The score of a guess.
/// How many letters were correct, how many letters were at the right place,
/// and the state of each individual letter.
/// The order of fields dictates the priority of comparing the scores.
/// `correct_places`, `correct_letters` and `letters` are compared in that order.
#[derive(Clone, Copy, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub struct GuessScore {
    /// The number of letters correctly guessed at the right place
    pub correct_places: u8,
    /// The number of correctly guessed letters which are at the wrong place
    pub correct_letters: u8,
    /// Scores of individual letters
    pub letters: [LetterScore; WORD_LENGTH],
    /// The index used for a handrolled "hash table"
    pub hash: u8,
}

impl GuessScore {
    /// Check if the guess is a winning move.
    /// All letters are at the correct places.
    pub fn is_winning(&self) -> bool {
        (self.correct_places as usize) == WORD_LENGTH
    }
}

/// Given a secret word and a player's guess, calculate the guess score.
pub fn guess_score(mut secret: Word, guess: Word) -> GuessScore {
    let mut letters = [LetterScore::Miss; WORD_LENGTH];

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

    // Since the numeric representation of `LetterScore` has three possible
    // values, we can pack five `LetterScore`s using base-3 representation
    // into a number [0..3**5), or [0..243). This value is then used as
    // an index into the storage array.
    let hash = (letters[0] as u8) * 81
        + (letters[1] as u8) * 27
        + (letters[2] as u8) * 9
        + (letters[3] as u8) * 3
        + (letters[4] as u8);

    GuessScore {
        correct_letters,
        correct_places,
        letters,
        hash,
    }
}

/// Domain-specific hash map for this task.
/// The algorithm of absurdle requires grouping possible secret words
/// into different buckets. We know ahead of the time the number of those
/// buckets and a mapping of keys to indices. This allows for efficient
/// grouping into buckets.
pub struct DSHashMap {
    /// Array of keys used in this map
    keys: Vec<Option<GuessScore>>,
    /// Array of values stored in the map
    values: Vec<Vec<Word>>,
}

impl DSHashMap {
    /// Construct an empty hash map.
    /// We pre-fill all the values with empty vectors to reduce branching
    /// and re-use allocations.
    pub fn new() -> Self {
        Self {
            keys: vec![None; 256],
            values: vec![Vec::new(); 256],
        }
    }

    /// Clear the hash map. This operation preserves all allocations.
    fn clear(&mut self) {
        for key in self.keys.iter_mut() {
            *key = None;
        }
        for val in self.values.iter_mut() {
            val.clear();
        }
    }

    /// Get value by key.
    fn get(&self, score: &GuessScore) -> &Vec<Word> {
        let pos = score.hash as usize;
        debug_assert!(self.keys[pos].is_some(), "missing key");
        debug_assert!(self.keys[pos] == Some(*score), "hash collision");
        &self.values[pos]
    }

    /// Get mutable reference to value by key
    fn get_mut(&mut self, score: &GuessScore) -> &mut Vec<Word> {
        let pos = score.hash as usize;
        match self.keys[pos] {
            Some(key) => debug_assert!(key == *score, "hash collision"),
            None => self.keys[pos] = Some(*score),
        }
        &mut self.values[pos]
    }
}

/// This is the core algorithm of Absurdle.
/// In short, for each step in the game, absurdle looks at which responses
/// it can give to the player's guess, and picks the "worst" response.
/// The "worst" response is determined first by the number of possible words
/// that match this response, second by the score ordering itself.
/// The function returns game's response to player's guess and updates the list
/// of possible remaining secret words.
pub fn get_rigged_response(
    // The hash map used for grouping the words by the corresponding response
    // This argument allows the caller to re-use allocation which avoids
    // unnecessary re-allocations to increase performance.
    buckets: &mut DSHashMap,
    // The list of possible secret words.
    secret_words: &mut Vec<Word>,
    // Player's guess word
    guess: Word,
) -> GuessScore {
    assert!(
        !secret_words.is_empty(),
        "The list of secret words cannot be empty"
    );

    // Group the possible answers into corresponding buckets using their scores
    for &word in secret_words.iter() {
        let score = guess_score(word, guess);
        buckets.get_mut(&score).push(word);
    }

    // Find the bucket with maximum number of entries or a "minimal" score
    let max_key = buckets
        .keys
        .iter()
        .filter_map(|&x| x)
        .max_by_key(|&score| (buckets.get(&score).len(), Reverse(score)))
        .expect("At least one key must be present, which means max must return Some");

    // Swap the worst bucket with the list of remaining secret words
    // This avoids unnecessary re-allocation
    let worst_bucket = buckets.get_mut(&max_key);
    core::mem::swap(secret_words, worst_bucket);

    // Keep the hash map clear for the next run of the function
    buckets.clear();

    max_key
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum GameError {
    WordNotAllowed,
    MustUseChar(u8),
    MustUseCharAt(u8, u8),
    MustNotUseChar(u8),
}

impl fmt::Display for GameError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            GameError::WordNotAllowed => {
                f.write_str("the word is not in the allowed list!")
            }
            GameError::MustUseChar(chr) => {
                write!(f, "the word must use char '{}'", chr as char)
            }
            GameError::MustUseCharAt(chr, pos) => {
                write!(f, "the word must use char '{}' at position {}",
                    chr as char, pos)
            }
            GameError::MustNotUseChar(chr) => {
                write!(f, "the word must not use char '{}'", chr as char)
            }
        }
    }
}

impl std::error::Error for GameError {}

pub struct RegularMode {
    buckets: DSHashMap,
    remaining: Vec<Word>,
    allowed: CheckAllowed,
}

impl RegularMode {
    /// Construct a game with the remaining word list and allowed word list.
    pub fn new(remaining: Vec<Word>, allowed: CheckAllowed) -> Self {
        Self {
            buckets: DSHashMap::new(),
            remaining,
            allowed,
        }
    }

    /// Update the game according to game input
    pub fn update(&mut self, word: Word) -> Result<GuessScore, GameError> {
        if !(self.allowed)(word) {
            return Err(GameError::WordNotAllowed);
        }
        Ok(get_rigged_response(
            &mut self.buckets,
            &mut self.remaining,
            word,
        ))
    }
}

pub type CheckAllowed = fn(word: Word) -> bool;

pub struct HardMode {
    buckets: DSHashMap,
    remaining: Vec<Word>,
    allowed: CheckAllowed,
    miss_chars: Vec<u8>,
    req_chars: Vec<u8>,
    req_place: Vec<(u8, usize)>,
}

impl HardMode {
    /// Construct a hard mode game with the remaining word list and allowed
    /// word list.
    pub fn new(remaining: Vec<Word>, allowed: CheckAllowed) -> Self {
        Self {
            buckets: DSHashMap::new(),
            remaining,
            allowed,
            miss_chars: Vec::new(),
            req_chars: Vec::new(),
            req_place: Vec::new(),
        }
    }

    /// Update the game according to game input
    pub fn update(&mut self, word: Word) -> Result<GuessScore, GameError> {
        if !(self.allowed)(word) {
            return Err(GameError::WordNotAllowed);
        }
        // Enforce hard mode requirements
        let mut cword = word;
        for &missing_chr in &self.miss_chars {
            if cword.contains(&missing_chr) {
                return Err(GameError::MustNotUseChar(missing_chr));
            }
        }
        for &(required_chr, pos) in &self.req_place {
            if cword[pos] != required_chr {
                cword[pos] = 0;
                return Err(GameError::MustUseCharAt(required_chr, (pos as u8) + 1));
            }
        }
        for &required_chr in &self.req_chars {
            if let Some(pos) = cword.iter().position(|&c| c == required_chr) {
                cword[pos] = 0;
            } else {
                return Err(GameError::MustUseChar(required_chr));
            }
        }

        // Update hard mode requirements
        let score = get_rigged_response(&mut self.buckets, &mut self.remaining, word);
        self.req_chars.clear();
        self.req_place.clear();

        for (index, (chr, score)) in word.iter().copied().zip(score.letters).enumerate() {
            match score {
                LetterScore::Miss => {
                    // Add missing chars if they were not used
                    if !self.req_chars.contains(&chr)
                        && !self.miss_chars.contains(&chr)
                        && !self.req_place.iter().any(|&(req, _)| chr == req)
                    {
                        self.miss_chars.push(chr);
                    }
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

    pub fn update(&mut self, word: Word) -> Result<GuessScore, GameError> {
        match self {
            Game::Regular(regular) => regular.update(word),
            Game::Hard(hard) => hard.update(word),
        }
    }
}

/// Check whether the specified path is valid for hard mode.
/// This doesn't actually check that the path is a valid solution for perf.
pub fn is_hard_solution(words: &[Word]) -> bool {
    fn all_allowed(_word: Word) -> bool {
        true
    }

    let mut words = words.iter();
    // Assume the last word is the solution, pop it off the end to use as secret.
    let target = *words.next_back().expect("at least one word is expected");

    // Construct a hard game mode with only target word in the secred word list.
    let mut hard_mode = HardMode::new(vec![target], all_allowed);
    // Run the game with the rest of the words.
    for &word in words {
        if hard_mode.update(word).is_err() {
            return false;
        }
    }

    true
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::words;

    #[test]
    fn check_guess_score() {
        use LetterScore::*;
        assert_eq!(
            guess_score(*b"ZORRO", *b"ZORRO").letters,
            [CorrectPlace; WORD_LENGTH],
            "Same word should have all letters in the correct place"
        );
        assert!(
            guess_score(*b"ZORRO", *b"ZORRO").is_winning(),
            "Same word should be winning"
        );
        assert_eq!(
            guess_score(*b"SISSY", *b"ZORRO").letters,
            [Miss; WORD_LENGTH],
            "All letters should be different"
        );
        assert_eq!(
            guess_score(*b"DRAIL", *b"LIDAR").letters,
            [CorrectLetter; WORD_LENGTH],
            "Complete anagram should have all letters correct"
        );
        // edge cases
        assert_eq!(
            guess_score(*b"AAAAB", *b"BBCCC").letters,
            [CorrectLetter, Miss, Miss, Miss, Miss],
            "The guess cannot re-use a letter more times than allowed 1"
        );
        assert_eq!(
            guess_score(*b"AAABB", *b"BBBCC").letters,
            [CorrectLetter, CorrectLetter, Miss, Miss, Miss],
            "The guess cannot re-use a letter more times than allowed 2"
        );
        assert_eq!(
            guess_score(*b"AAABB", *b"BBCCC").letters,
            [CorrectLetter, CorrectLetter, Miss, Miss, Miss],
            "The score must show that the letter repeats"
        );
        assert_eq!(
            guess_score(*b"ABABA", *b"BBCCC").letters,
            [CorrectLetter, CorrectPlace, Miss, Miss, Miss],
            "The score must prioritize correct places first"
        );
    }

    fn valid_guess(word: Word) -> bool {
        words::POSSIBLE_WORDS.contains(&word) || words::IMPOSSIBLE_WORDS.contains(&word)
    }

    #[test]
    fn check_regular_game() {
        use LetterScore::*;
        let mut game = Game::new(words::POSSIBLE_WORDS.into(), valid_guess, false);
        assert_eq!(
            game.update(*b"ZZZZZ"),
            Err(GameError::WordNotAllowed),
            "must reject non-listed words"
        );
        let path = [
            (*b"LIDAR", [Miss; WORD_LENGTH]),
            (*b"STONY", [Miss, CorrectLetter, Miss, Miss, Miss]),
            (*b"BECKE", [Miss; WORD_LENGTH]),
            (*b"THUMP", [CorrectPlace; WORD_LENGTH]),
        ];
        for (word, ans) in path {
            let score = game.update(word).expect("should accept a valid move");
            assert_eq!(score.letters, ans, "should produce correct response");
        }
    }

    #[test]
    fn check_hard_game() {
        use LetterScore::*;
        let mut game = Game::new(words::POSSIBLE_WORDS.into(), valid_guess, true);
        assert_eq!(
            game.update(*b"ZZZZZ"),
            Err(GameError::WordNotAllowed),
            "must reject non-listed words"
        );

        let score = game.update(*b"AEONS").expect("should accept a valid move");
        assert_eq!(score.letters, [Miss, CorrectLetter, Miss, Miss, Miss]);
        assert_eq!(
            game.update(*b"ADORN"),
            Err(GameError::MustNotUseChar(b'A')),
            "must reject words with chars that got rejected"
        );
        assert_eq!(
            game.update(*b"CRUFT"),
            Err(GameError::MustUseChar(b'E')),
            "must reject words with chars that should be present"
        );

        let _ = game.update(*b"BRIDE").expect("should accept a valid move");
        let _ = game.update(*b"CLEEP").expect("should accept a valid move");
        let score = game.update(*b"EMPTY").expect("should accept a valid move");
        assert!(score.is_winning(), "must be a winning move");
    }

    #[test]
    fn check_is_hard_solution() {
        let hard_solutions = [
            &[*b"MESON", *b"EARED", *b"TUILE", *b"CHECK"],
            &[*b"AEONS", *b"BRIDE", *b"CLEEP", *b"EMPTY"],
            &[*b"AGLEY", *b"TRONK", *b"CHIBS", *b"VIVID"],
        ];
        for (index, solution) in hard_solutions.into_iter().enumerate() {
            assert!(
                is_hard_solution(solution),
                "should accept hard solution {}",
                index
            );
        }

        let soft_solutions = [
            &[*b"ABYES", *b"GLOUT", *b"CHIMP", *b"DONOR"],
            &[*b"LADES", *b"FOUNT", *b"CHIPS", *b"PYGMY"],
            &[*b"ADORN", *b"YULES", *b"CHIRT", *b"FEMME"],
        ];
        for (index, solution) in soft_solutions.into_iter().enumerate() {
            assert!(
                !is_hard_solution(solution),
                "should not accept soft solution {}",
                index
            );
        }
    }
}
