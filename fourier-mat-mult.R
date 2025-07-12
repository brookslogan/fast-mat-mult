
library(magrittr)

I <- 3
J <- 4
K <- 5

withr::with_rng_version("3.5", withr::with_seed(4775832, {
  X <- matrix(rpois(I*J, 10), I, J)
  Y <- matrix(rpois(J*K, 10), J, K)
  # Y <- matrix(seq_len(J*K), J, K)
  # Y <- matrix(, J, K) %>% {.[seq_along(.)] <- as.numeric(paste0(row(.), col(.))); .}
}))

omegaIJK <- exp(2*pi*1i/(I*J*K))

# i, IjK, Ik

# careless 1-index-based seems to work

# Xhat <- sapply(seq_len(K), function(khat) {
#   sapply(seq_len(I), function(ihat) {
#     is <- row(X)
#     js <- col(X)
#     sum(X * omegaIJK^((is + I*js*K))*(ihat + I*khat))
#   })
# })

# Yhatp <- sapply(seq_len(K), function(khat) {
#   sapply(seq_len(I), function(ihat) {
#     js <- row(Y)
#     ks <- col(Y)
#     sum(Y * omegaIJK^(-(I*js*K + I*ks))*(ihat + I*khat))
#   })
# })

# XY <- X %*% Y
# XYhatpp <- sapply(seq_len(K), function(khat) {
#   sapply(seq_len(I), function(ihat) {
#     is <- row(XY)
#     ks <- col(XY)
#     sum(XY * omegaIJK^((is + I*ks))*(ihat + I*khat))
#   })
# })

# (Xhat * Yhatp / XYhatpp) %>% {. / .[[1]]} %>% {. / ((4:18)/4)}

# 0-index-based

# Xhat <- sapply(seq_len(K) - 1L, function(khat) {
#   sapply(seq_len(I) - 1L, function(ihat) {
#     is <- row(X) - 1L
#     js <- col(X) - 1L
#     sum(X * omegaIJK^((is + I*js*K))*(ihat + I*khat))
#   })
# })

# Yhatp <- sapply(seq_len(K) - 1L, function(khat) {
#   sapply(seq_len(I) - 1L, function(ihat) {
#     js <- row(Y) - 1L
#     ks <- col(Y) - 1L
#     sum(Y * omegaIJK^(-(I*js*K + I*ks))*(ihat + I*khat))
#   })
# })

# XY <- X %*% Y
# XYhatpp <- sapply(seq_len(K) - 1L, function(khat) {
#   sapply(seq_len(I) - 1L, function(ihat) {
#     is <- row(XY) - 1L
#     ks <- col(XY) - 1L
#     sum(XY * omegaIJK^((is + I*ks))*(ihat + I*khat))
#   })
# })

# stopifnot(dplyr::near((Xhat * Yhatp)[[1L]], XYhatpp[[1L]]))
# stopifnot(all(dplyr::near(((Xhat * Yhatp) / XYhatpp) %>% {. / .[[2L]]} %>% {. / (seq_along(.) - 1L)} %>% {.[-1]}, 1)))

# NOTE for DFT, omega exponents above probably need changed to be negative

# 0-index-based, matching DFT power signs

Xhat <- sapply(seq_len(K) - 1L, function(khat) {
  sapply(seq_len(I) - 1L, function(ihat) {
    is <- row(X) - 1L
    js <- col(X) - 1L
    sum(X * omegaIJK^(-(is + I*js*K))*(ihat + I*khat))
  })
})

Yhatp <- sapply(seq_len(K) - 1L, function(khat) {
  sapply(seq_len(I) - 1L, function(ihat) {
    js <- row(Y) - 1L
    ks <- col(Y) - 1L
    sum(Y * omegaIJK^((I*js*K + I*ks))*(ihat + I*khat))
  })
})

XY <- X %*% Y
XYhatpp <- sapply(seq_len(K) - 1L, function(khat) {
  sapply(seq_len(I) - 1L, function(ihat) {
    is <- row(XY) - 1L
    ks <- col(XY) - 1L
    sum(XY * omegaIJK^(-(is + I*ks))*(ihat + I*khat))
  })
})

stopifnot(dplyr::near((Xhat * Yhatp)[[1L]], XYhatpp[[1L]]))
stopifnot(all(dplyr::near(((Xhat * Yhatp) / XYhatpp) %>% {. / .[[2L]]} %>% {. / (seq_along(.) - 1L)} %>% {.[-1]}, 1)))

# TODO check if Y time reversal is done correctly

# TODO explain the fixups needed

# TODO try to figure out ways to derive hat matrices more quickly

# TODO figure out reverse transform

Xspread <-
  array(0, c(I, K, J)) %>%
  {.[,1L,] <- X; dim(.) <- NULL; .}

Yspread <-
  array(0, c(I, K, J)) %>%
  {.[1L,,] <- t(Y); dim(.) <- NULL; .}

XYspread <-
  array(0, c(I, K, J)) %>%
  {.[,,1L] <- XY; dim(.) <- NULL; .}

omegaIK <- exp(2*pi*1i/(I*K))
stopifnot(dplyr::near(omegaIK, omegaIJK^J))

XYhatqq <- fft(XYspread)

XYhatqq[J*(row(XY) - 1L + I*(col(XY) - 1L)) + 1L]
XYhatpp
# FIXME wrong



(Xspread * Yspread) %>%
  # {dim(.) <- c(I*K, J); rowSums(.)} %>%
  sum() %>%
  {}
XY[[1L]]

(Xspread * Yspread[((seq_along(Yspread) - 1L) - 2L) %% length(Yspread) + 1L]) %>%
  sum() %>%
  {}
XY[[1L + 2L]]

dplyr::near(XY,
            sapply(seq_along(XY) - 1L, function(shift) {
              (Xspread * Yspread[((seq_along(Yspread) - 1L) - shift) %% length(Yspread) + 1L]) %>%
                sum() %>%
                {}
            }))
# FIXME wrong
XY
sapply(seq_along(XY) - 1L, function(shift) {

    sum() %>%
    {}
}) %>%
  `dim<-`(dim(XY))

cbind(Xspread, Yspread)

# This may be connected to initial intuition in notebook sketch that Y->Yhatp
# transformation should only negate part of exponent.

Yspread2 <-
  array(0, c(I, K, J)) %>%
  {.[1L,,] <- t(Y[,rev(seq_len(K))]) %>% {.[(seq_along(.) - 1L + K - 1L)%%length(.)+1L]}; dim(.) <- NULL; .}

sapply(seq_along(XY) - 1L, function(shift) {
  (Xspread * Yspread2[((seq_along(Yspread2) - 1L) - shift) %% length(Yspread2) + 1L]) %>%
    sum() %>%
    {}
}) %>%
  `dim<-`(dim(XY))

cbind(Xspread, Yspread2)

stopifnot(all(dplyr::near(XY,
                          sapply(seq_along(XY) - 1L, function(shift) {
                            (Xspread * Yspread2[((seq_along(Yspread2) - 1L) - shift) %% length(Yspread2) + 1L]) %>%
                              sum() %>%
                              {}
                          }))))
stopifnot(all(dplyr::near(XY, convolve(Xspread, Yspread2)[seq_len(I*K)])))

# FIXME preceding hat lineup all seems bogus...
Xhat / Xhat[2]

fft(Xspread) * fft(c(Yspread2[[1L]], rev(tail(Yspread2, -1L)))) / fft(XYspread)
fft(Xspread) * fft(Yspread2) / fft(XYspread)
fft(Xspread) * fft(rev(Yspread2)) / fft(XYspread)

fft(XYspread)

stopifnot(all.equal(XYspread[seq_len(I*K)], as.vector(XY)))

# fft of XY entries is given by taking every Jth element of fft of XYspread
stopifnot(all.equal(fft(XYspread)[(seq_len(I*K)-1L) * J + 1L], fft(as.vector(XY))))

fft(as.vector(X))
fft(Xspread) # contains shared entries, but not simply at the indices we'd need to read off XY transform

fft(c(2,2))

fft(c(2,2,0,0,0,0))

fft(c(2,0,0,2,0,0))

stopifnot(all(dplyr::near(fft(c(2,2,0,0,0,0)), c(4, 3 - sqrt(3)*1i, 1 - sqrt(3)*1i, 0, 1+sqrt(3)*1i, 3+sqrt(3)*1i))))

exp(-2*pi*1i / 3)^(1:3) * 2 # potentially related

exp(-2*pi*1i / 3)^(1:6)

cumsum(vctrs::vec_interleave(fft(c(2,2)), 0, 0) + c(0, head(exp(-2*pi*1i / 3)^(1:3) * 2, - 1)))

fft(c(1,3))
fft(c(1,3,0,0,0,0))
fft(c(1,0,0,3,0,0))

fft(c(5,3))
fft(c(5,3,0,0,0,0))
fft(c(5,0,0,3,0,0))

fft(c(1,0))
fft(c(1,0,0,0,0,0))
fft(c(1,0,0,0,0,0))

fft(c(0,1))
fft(c(0,1,0,0,0,0))
fft(c(0,0,0,1,0,0))

convolve(exp(2*pi*1i/6*0:2), c(1,0,0))

# TODO check geometric progression formula

# TODO try to word as convolution in either domain?

# TODO also try comparing summations to see if can factor out some sum-of-finer-root-powers as a multiplier

# TODO alternatively, consider it as summing multiple sparse combs; see if comb and comb shifts are nice at all...

fft(c(1,0,4,0))
fft(c(1,0,0,4,0,0))
fft(c(1,0,0,0,4,0,0,0))
fft(c(0,1,0,0,0,4,0,0))
stopifnot(all.equal(fft(c(0,1,0,0,0,4,0,0)), fft(c(1,0,0,0,4,0,0,0)) * exp(-2*pi*1i/8 * 0:7)))

fft(Xspread) # at least some mirror kind of symmetry
fft(Yspread) # periodic + mirrored within periodic
fft(XYspread) # at least mirror
fft(as.vector(XY)) # pulling from XYspread every J

fft(t(X)) # manipulate based on shifts & linearity?

fft(Xspread)[seq(1, I*J*K, by = J)]

Xcomb0 <- X
Xcomb0[-(0L + 1L),] <- 0

Xcomb0spread <-
  array(0, c(I, K, J)) %>%
  {.[,1L,] <- Xcomb0; dim(.) <- NULL; .}

Xcomb0spread_hat <- fft(Xcomb0spread) %>% round()

Xteeth0_hat <- fft(X[(0L + 1L), ])

stopifnot(all.equal(rep(Xteeth0_hat, I*K), Xcomb0spread_hat))

Xcomb1 <- X
Xcomb1[-(1L + 1L),] <- 0

Xcomb1spread <-
  array(0, c(I, K, J)) %>%
  {.[,1L,] <- Xcomb1; dim(.) <- NULL; .}

Xcomb1spread_hat <- fft(Xcomb1spread) %>% round()

Xteeth1_hat <- fft(X[(1L + 1L), ])

Xcomb10 <- Xcomb1
Xcomb10[0 + 1,] <- Xcomb10[1 + 1,]
Xcomb10[1 + 1,] <- 0

Xcomb10spread <-
  array(0, c(I, K, J)) %>%
  {.[,1L,] <- Xcomb10; dim(.) <- NULL; .}

Xcomb10spread_hat <- fft(Xcomb10spread)

Xcomb1spread_hat/Xcomb10spread_hat  - omegaIJK^-(seq_len(I*J*K) - 1L)
# ... not exact??

abs(Xcomb1spread_hat) / abs(Xcomb10spread_hat)

## FIXME begin copied from help(fft)

## Slow Discrete Fourier Transform (DFT) - e.g., for checking the formula
fft0 <- function(z, inverse=FALSE) {
  n <- length(z)
  if(n == 0) return(z)
  k <- 0:(n-1)
  ff <- (if(inverse) 1 else -1) * 2*pi * 1i * k/n
  vapply(1:n, function(h) sum(z * exp(ff*(h-1))), complex(1))
}

## end copied from help(fft)

Xcomb1spread_hat / fft0(Xcomb1spread)
# XXX !!! numerical issues this large?!

Xcomb1spread_hat / rep(Xteeth1_hat, I*K)
omegaIJK^-(seq_len(I*J*K) - 1L)

stopifnot(all.equal(fft0(Xcomb1spread), rep(Xteeth1_hat, I*K) * omegaIJK^-(seq_len(I*J*K) - 1L)))
