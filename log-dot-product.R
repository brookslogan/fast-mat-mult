
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
