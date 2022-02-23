set term png
set title 'Distribution of all paths by the number of remaining words at depth 2'

set ylabel 'number of paths'

set xlabel 'remaining words'
set output 'distribution-depth-2.png'
plot \
    '../results/distribution-all-depth-2.dat' using 1:($1 <= 40 ? $2 : NaN) with filledcurve y1=0 title 'explored paths', \
    '../results/distribution-all-depth-2.dat' with lines title 'distribution'

set logscale x 10
set xlabel 'logscale remaining words'
set output 'distribution-depth-2-log.png'
plot \
    '../results/distribution-all-depth-2.dat' using 1:($1 <= 40 ? $2 : NaN) with filledcurve y1=0 title 'explored paths', \
    '../results/distribution-all-depth-2.dat' with lines title 'distribution'
