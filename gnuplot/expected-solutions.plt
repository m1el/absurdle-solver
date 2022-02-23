set term png
set title 'expected distribution of solutions'

set ylabel 'number of paths'

set xrange [0:50]
set xlabel 'logscale remaining words'
set output 'expected-solutions.png'
plot '../results/distribution-all-depth-2.dat' using 1:($2 * exp(0.494019*(7-$1))) \
    lt rgb '#f3722c' with lines title 'expected distribution', \
    '../results/all-words-distribution-grouped.dat' lt rgb '#43aa8b' with lines title 'actual distribution'
