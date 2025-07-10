
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
  (Xspread * Yspread[((seq_along(Yspread) - 1L) - shift) %% length(Yspread) + 1L]) %>%
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

# FIXME preceding hat lineup all seems bogus...
Xhat / Xhat[2]
