
library(magrittr)

I <- 3
J <- 4
K <- 5

withr::with_rng_version("3.5", withr::with_seed(4775832, {
  X <- matrix(rpois(I*J, 10), I, J)
  Y <- matrix(rpois(J*K, 10), J, K)
}))

omegaIJK <- exp(2*pi*1i/(I*J*K))

# i, IjK, Ik

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
# Zhatpp <- sapply(seq_len(K), function(khat) {
#   sapply(seq_len(I), function(ihat) {
#     is <- row(XY)
#     ks <- col(XY)
#     sum(XY * omegaIJK^((is + I*ks))*(ihat + I*khat))
#   })
# })

# (Xhat * Yhatp / Zhatpp) %>% {. / .[[1]]} %>% {. / ((4:18)/4)}

Xhat <- sapply(seq_len(K) - 1L, function(khat) {
  sapply(seq_len(I) - 1L, function(ihat) {
    is <- row(X) - 1L
    js <- col(X) - 1L
    sum(X * omegaIJK^((is + I*js*K))*(ihat + I*khat))
  })
})

Yhatp <- sapply(seq_len(K) - 1L, function(khat) {
  sapply(seq_len(I) - 1L, function(ihat) {
    js <- row(Y) - 1L
    ks <- col(Y) - 1L
    sum(Y * omegaIJK^(-(I*js*K + I*ks))*(ihat + I*khat))
  })
})

XY <- X %*% Y
Zhatpp <- sapply(seq_len(K) - 1L, function(khat) {
  sapply(seq_len(I) - 1L, function(ihat) {
    is <- row(XY) - 1L
    ks <- col(XY) - 1L
    sum(XY * omegaIJK^((is + I*ks))*(ihat + I*khat))
  })
})

((Xhat * Yhatp) / Zhatpp) %>% {. / .[[2L]]} %>% {. / 0:14}
