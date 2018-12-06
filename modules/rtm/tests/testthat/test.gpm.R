library(PEcAnRTM)
library(testthat)
context("Generalized plate model")

data(dataSpec_prospectd)
true_param <- defparam("prospect_d")

N <- true_param["N"]
k_coefs <- as.matrix(true_param[-1])
refractive <- dataSpec_prospectd[, "refractive"]

k <- (dataSpec_prospectd[, -(1:2)] %*% k_coefs) / N

rtmat <- generalized_plate_model(k, refractive, N)
prosp <- prospect(true_param, "D")

test_that(
  "Generalized plate model returns same values as PROSPECT", {
    expect_lt(sum(rtmat - prosp), 1e-5)
  }
)
