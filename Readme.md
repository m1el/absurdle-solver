# Absurdle solver

## Overview

The goal of this program is to find 4-word solutions to [Absurdle](https://qntm.org/absurdle) by [qntm](https://qntm.org/).

In short, Absurdle is a game in which a player needs to guess a 5-letter secret
word picked by the computer.  The player can check a word and the computer
answers with a score of the provided guess.  The score says which letters
were in the right place and which letters were right, but in the wrong place.
The twist of Absurdle that the computer doesn't pick a single secret word,
but has a pool of possible words, and narrows it down as the player uses
more guesses.  The computer tries to do this in the nastiest way possible,
providing the "worst" possible score for the remaining pool of answers.

## Solution explanaion

Since each player guess narrows down the pool of possible answers,
the last guess only confirms that the pool has a size 1.
So it's possible to rephrase the problem as "how to narrow down the pool of
possible words to size 1?".

An exhaustive search (which can be done by running `two-word-distr` subcommand)
shows that it's not possible to narrow down the remaining words with two guesses.
This eliminates the possibility of finding 3-word paths to Absurdle.

This program (`find-solutions` subcommand) runs multiple threads,
each performing a depth first search through all 3-word paths.
If a path reduces the pool to size 1, it's treated as a valid solution.

There are `12972` allowed guesses, which means there are `choose(12972, 3)`,
or `2_182_329_761_640` paths of size 3.
(Eliminating the paths with repeating guesses)

The search space can be reduced with a pruning criteria.  At the second step,
we check the size of the pool, and if the pool is too large (more than 40 words),
this path is not explored further.  This reduces the search space to `67_643_193_260`
paths, or \~3% of the initial search space.

## Subcommands

The solver provides multiple subcommands to explore and test Absurdle search space.

- `play [hard]`: Play the game interactively in the console.  Optionally add "hard" to play hard mode.
  Hard mode requires you to strictly use all the information previously acquired in the game.
- `find-solutions [--prune <number>] [<starting word>]`: Find 4-word solutions.  Optionally provide with the starting word.
- `two-word-distr`: Calculate the distribution of remaining pool sizes for all combinations of two starting words.
- `solution-distr`: Take solutions from stdin, print their distribution by the first two starting words.
- `filter-hard`: Take solutions from stdin, only print valid hard mode solutions.
- `bench-guess-score`: Benchmark how long it takes to calculate score for one word guess.
- `find-all-scores`: A test function to check whether the hash function is fitting for the purpose.

## License

The MIT License