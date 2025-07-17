
# Think about a^T b as summing diagonal of b a^T.  Can we subtract
# away off-diagonal rectangles of b a^T efficiently enough to get
# log-time result with other-vector-independent linearithmic/friendly
# transformation?

#    b1 b2 b3 b4 b5 b6 b7 b8
# a1  1  C  B  B  A  A  A  A
# a2  C  1  B  B  A  A  A  A
# a3  B  B  1  C  A  A  A  A
# a4  B  B  C  1  A  A  A  A
# a5  A  A  A  A  1  C  B  B
# a6  A  A  A  A  C  1  B  B
# a7  A  A  A  A  B  B  1  C
# a8  A  A  A  A  B  B  C  1

# computing C sums outlined here seems potentially unfriendly.

# Something with parity?

# Some sort of inclusion-exclusion-inclusion-exclusion-etc. approach?

# Rectangles + inclusion-exclusion?


# Thought there was something about this sort of pattern extended
as.matrix(c(1,-1,1,-1)) %*% t(as.matrix(c(1,-1,1,-1))) + as.matrix(c(1,1,-1,-1)) %*% t(as.matrix(c(1,1,-1,-1)))
# though maybe with alternating +s and -s


# parity... something about subtracting away where index bits
# mismatch?  or getting bits representing the number of bit mismatches
# in the indices?




tcrossprod(c(+1,-1,+1,-1), c(+1,-1,+1,-1))
tcrossprod(c(+1,+1,-1,-1), c(+1,+1,-1,-1))
tcrossprod(c(+1,-1,+1,-1), c(+1,+1,-1,-1))
tcrossprod(c(+1,+1,-1,-1), c(+1,-1,+1,-1))

tcrossprod(c(+1,-1,+1,-1)+1, c(+1,+1,-1,-1))

# m({mismatch on bit 1} U {mismatch on bit 2})
# = m({mismatch on bit 1}) + m({mismatch on bit 2}) - m({mismatch on bit 1 and 2})

# tcrossprod(c(+1,-1,+1,-1), c(+1,-1,+1,-1)) +
# tcrossprod(c(+1,+1,-1,-1), c(+1,+1,-1,-1)) -
# revdiag...


# TODO think about ways to simultaneously refine rectangles summing to
# approximate triangle incorporating each finer bit

# TODO think about starting with checkerboard, subtracting away
# off-checkerboards?  definitely feels like sublevel results are
# important

1 - (tcrossprod(-c(+1,-1,+1,-1), c(+1,-1,+1,-1)) +
  tcrossprod(-c(+1,+1,-1,-1), c(+1,+1,-1,-1)) -
  ((1+tcrossprod(-c(+1,-1,+1,-1), c(+1,-1,+1,-1))) *
  (1+tcrossprod(-c(+1,+1,-1,-1), c(+1,+1,-1,-1)))-2)/2)

(1+tcrossprod(c(+1,-1,+1,-1), c(+1,-1,+1,-1))) *
  (1+tcrossprod(c(+1,+1,-1,-1), c(+1,+1,-1,-1)))/4

# What is multiplication analogue in this case?
# * given numbers
# * given the 0th bit of each number where we are trying to find 0th bit of result

# Or is this another op/way to get union/intersection?

# This path actually based on cardinality calcs, but * for inverse
# symdiff and +1/+2/etc. may be suspect.
m1 <- tcrossprod(-c(+1,-1,+1,-1), c(+1,-1,+1,-1))
m2 <- tcrossprod(-c(+1,+1,-1,-1), c(+1,+1,-1,-1))
(1 - ((m1+m2+2)/2 - (m1*m2+1)/2))/2
# Both multiplication and addition need to be mapped to analogues here
# and above.  If they can be.

x <- c(2,3,5,1)
y <- c(5,4,1,2)

x1 <- sum(x[c(1,3)]) - sum(x[c(2,4)])
y1 <- sum(y[c(1,3)]) - sum(y[c(2,4)])
x2 <- sum(x[c(1,2)]) - sum(x[c(3,4)])
y2 <- sum(y[c(1,2)]) - sum(y[c(3,4)])

t(x) %*% m1 %*% as.matrix(y)
-x1*y1

t(x) %*% m2 %*% as.matrix(y)
-x2*y2
x2*(sum(y)-y2)

m1 == -1 & m2 == -1

m11 <- m1
m12 <- tcrossprod(-c(+1,-1,+1,-1), c(+1,+1,-1,-1))
m21 <- tcrossprod(-c(+1,+1,-1,-1), c(+1,-1,+1,-1))
m22 <- m2

# ... multiplication still needs attention / is the spanner in the works


# Diagonal extraction also calls to mind ideas about sum_{j=1}^p
# poly(omega_p) scaling entries of x by omega^{0:(p-1)} and y by
# inverse, though this is probably covered in Fourier brainstorming &
# notes.


tcrossprod(bitwAnd(0:7, 1), bitwAnd(0:7, 1))
tcrossprod(bitwAnd(0:7, 2), bitwAnd(0:7, 2))
tcrossprod(bitwAnd(0:7, 4), bitwAnd(0:7, 4))

tcrossprod(bitwAnd(0:7, 1)/1*2-1, bitwAnd(0:7, 1)/1*2-1)
tcrossprod(bitwAnd(0:7, 2)/2*2-1, bitwAnd(0:7, 2)/2*2-1)
tcrossprod(bitwAnd(0:7, 4)/4*2-1, bitwAnd(0:7, 4)/4*2-1)

(tcrossprod(bitwAnd(0:7, 1)/1*2-1, bitwAnd(0:7, 1)/1*2-1)+1)/2
(tcrossprod(bitwAnd(0:7, 2)/2*2-1, bitwAnd(0:7, 2)/2*2-1)+1)/2
(tcrossprod(bitwAnd(0:7, 4)/4*2-1, bitwAnd(0:7, 4)/4*2-1)+1)/2

(tcrossprod(bitwAnd(0:7, 1)/1*2-1, bitwAnd(0:7, 1)/1*2-1)+1)/2 *
(tcrossprod(bitwAnd(0:7, 2)/2*2-1, bitwAnd(0:7, 2)/2*2-1)+1)/2 *
(tcrossprod(bitwAnd(0:7, 4)/4*2-1, bitwAnd(0:7, 4)/4*2-1)+1)/2

tcrossprod(c(1,1,1,1,0,0,0,0), c(0,0,0,0,1,1,1,1))

tcrossprod(c(1,1,0,0,0,0,0,0), c(0,0,1,1,0,0,0,0)) +
tcrossprod(c(0,0,0,0,1,1,0,0), c(0,0,0,0,0,0,1,1))

tcrossprod(c(1,1,0,0,1,1,0,0), c(0,0,1,1,0,0,1,1))

# Look at parity stuff to try to get omega^{# bit mismatches}
# coefficients, so can sum poly(omega) evals only over some prime
# ~log(J) powers?  Or perhaps omega^{first bit mismatch} +
# omega^{second bit mismatch} + ... to make matching circle around to
# zero, and try to get "complement" somehow? (might just be scaling by
# number of mismatches...). Poly approach having separate variable for
# each bit seems to just become FFT.  Something with products rather
# than sums?  Might end up pairing any chain of entries with a bit
# match in each etc.... any way to combine with subsetting to try to
# avoid?
