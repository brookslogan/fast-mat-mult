
# It seems like FFT with nonbinary radix may form&combine subresults in a way
# similar/equivalent to needed for shifted-comb-summation in fourier-mat-mult
# and/or multidimensional DFT. That raises an idea for a choice of radix that's
# a bit larger than might be expected, around sqrt(n). The work might
# potentially look something like
#
# f(n) = sqrt(n) f(sqrt(n)) + a n ???
#
# FIXME TODO cite https://www.wolframalpha.com/input?i=solve+f%28n%29+%3D+sqrt%28n%29+f%28sqrt%28n%29%29+%2B+n
#
# Wolfram Alpha says this is n log log n; asymptotic improvement over existing offerings?
#
# Could also consider higher-dimensional analogues, see if complexity differs at all.
