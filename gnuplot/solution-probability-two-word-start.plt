set term png
set title 'Proportion of starting pairs that lead to a solution as f(depth 2 words)'

set ylabel 'logscale proportion of pairs'
set xlabel 'remaining words'
set logscale y 10
set output 'proportion-of-solutions.png'
plot '../results/propotion-of-solutions.txt' using 1:($3/$2) lt rgb '#43aa8b' with lines title 'proportion of paths'
