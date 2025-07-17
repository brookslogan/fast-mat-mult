
# Think about a^T b as summing diagonal of b a^T.  Can we subtract
# away off-diagonal rectangles of b a^T efficiently enough to get
# log-time result with other-vector-independent linear/friendly
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
