## New PROSPECT inversion
library(Rcpp)
load("c_inversion/cp4.Rdata")
wl <- 2101

sourceCpp("c_inversion/prospect.cpp")

load("nimble/pinv_nimble_testdat.Rdata")
z <- pinvbayes(20, obs.spec)
