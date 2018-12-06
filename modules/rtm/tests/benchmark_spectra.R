library(PEcAnRTM)
library(microbenchmark)

prior <- prospect_bt_prior("D")
sampler <- function() prior$sampler()[1:7]

m1 <- function() prospect(sampler(), "D")[, 1]
m2 <- function() spectra(prospect(sampler(), "D")[, 1], 400:2500)

bm <- microbenchmark(m1, m2, times = 100000)
summary(bm)
