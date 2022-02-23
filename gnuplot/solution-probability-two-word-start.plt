set term png
set title 'Proportion of valid paths as f(words at depth 2)'

set ylabel 'logscale proportion of valid paths'
set xlabel 'remaining words at depth 2'
set logscale y 10
set output 'proportion-of-solutions.png'

f(x)= a*(7-x)
fit f(x) '../results/propotion-of-solutions.txt' using 1:(log($3/$2)) via a
plot '../results/propotion-of-solutions.txt' using 1:($3/$2) lt rgb '#43aa8b' with lines title 'proportion of paths', \
    exp(f(x)) lt rgb '#f3722c' title 'approximation'
