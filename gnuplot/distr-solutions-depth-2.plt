set term png
set title 'Distribution of solutions by the number of remaining words at depth 2'

set ylabel 'number of solutions'
set xlabel 'remaining words'
set output 'solutions-distribution-depth-2.png'
plot '../results/all-words-distribution.dat' with lines title 'all words', \
    '../results/common-distribution.dat' with lines title 'common words'

set title 'Distribution of solution groups by the number of remaining words at depth 2'

set ylabel 'number of solution groups'
set xlabel 'remaining words'
set output 'solutions-grouped-distribution-depth-2.png'
plot '../results/all-words-distribution-grouped.dat' with lines title 'all words', \
    '../results/common-distribution-grouped.dat' with lines title 'common words'
