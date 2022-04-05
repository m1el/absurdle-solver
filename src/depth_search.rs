use crate::game::{get_rigged_response, DSHashMap, Word};
use crate::words;
use crate::EXPECTED_PATH_LEN;
use core::sync::atomic::{AtomicU64, Ordering as AtomicOrdering};

/// Stats recorded during search.
pub struct DepthSearchStats {
    /// Counter for pruned paths.
    pub pruned: AtomicU64,
    /// Counter for processed paths.
    pub processed: AtomicU64,
    /// Number of currently running threads.
    pub running: AtomicU64,
}

impl DepthSearchStats {
    /// Construct clean stats
    pub fn new() -> Self {
        Self {
            pruned: AtomicU64::new(0),
            processed: AtomicU64::new(0),
            running: AtomicU64::new(0),
        }
    }
}

/// Encapsulation of depth-first search logic
pub struct DepthSearch<'a> {
    /// Re-usable storage for `get_rigged_response`
    buckets: DSHashMap,
    /// Current list of remaining words for the recursive descent
    remaining: Vec<Word>,
    /// Current search path
    path: Vec<Word>,
    /// Prune size at depth 2
    prune: usize,
    /// statistics
    stats: &'a DepthSearchStats,
}

impl<'a> DepthSearch<'a> {
    /// Construct new depth search, with given parameters.
    /// This function will take care of reducing the word list according to
    /// the provided path.
    pub fn new(
        word_list: &[Word],
        path: Vec<Word>,
        prune: usize,
        stats: &'a DepthSearchStats,
    ) -> Self {
        let mut remaining = word_list.to_vec();
        let mut buckets = DSHashMap::new();
        for &guess in path.iter() {
            get_rigged_response(&mut buckets, &mut remaining, guess);
        }
        Self {
            buckets,
            path,
            remaining,
            prune,
            stats,
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

        let all_words = words::POSSIBLE_WORDS.iter().chain(words::IMPOSSIBLE_WORDS);
        if self.path.len() == PRUNE_DEPTH && self.remaining.len() > self.prune {
            // Hoping for the codegen to replace this with a constant
            let word_count = all_words.count();

            #[allow(clippy::assertions_on_constants)]
            {
                assert!(
                    MAX_SEARCH_DEPTH == PRUNE_DEPTH + 1,
                    "Requirement for calculating combinations for pruned words."
                )
            };
            // Since there are 2 words in the path, there are 2 fewer words
            // to be explored than the total number of words.
            self.stats
                .pruned
                .fetch_add((word_count - PRUNE_DEPTH) as u64, AtomicOrdering::Relaxed);
            return;
        }

        assert!(
            self.path.len() <= MAX_SEARCH_DEPTH,
            "Path length should not exceed MAX_SEARCH_DEPTH"
        );

        if self.path.len() == MAX_SEARCH_DEPTH {
            // Check that there is only one remaining word left, and call
            // the sink with the solution path.
            if self.remaining.len() == 1 {
                self.path.push(self.remaining[0]);
                sink(&self.path);
                self.path.pop();
            }

            self.stats.processed.fetch_add(1, AtomicOrdering::Relaxed);
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
