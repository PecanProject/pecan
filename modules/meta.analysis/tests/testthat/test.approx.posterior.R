context("test approx.posterior function")

load("data/trait.mcmc.RData")
load("data/prior.distns.RData")

test_that("test data are as expected", {
  expect_equal(names(trait.mcmc),
               c("quantum_efficiency", "leaf_respiration_rate_m2",
                 "stomatal_slope.BB", "SLA", "Vcmax"))
  expect_is(trait.mcmc, "list")
  expect_is(trait.mcmc[[1]], "mcmc.list")
  expect_is(prior.distns, "data.frame")
  expect_equal(dim(prior.distns), c(11, 4))
  expect_is(prior.distns$distn, "character")
  expect_is(prior.distns$parama, "numeric")
  expect_is(prior.distns$paramb, "numeric")
})

test_that("approx.posterior function returns expected dataframe given test data", {
  test.ap <- approx.posterior(trait.mcmc, priors = prior.distns)
  expect_is(test.ap, "data.frame")
  expect_equal(dim(test.ap), c(11, 4))
  expect_equal(dim(test.ap), dim(prior.distns))
  expect_equal(colnames(test.ap), colnames(prior.distns))
  expect_equal(rownames(test.ap), rownames(prior.distns))
  expect_equal(test.ap$n, prior.distns$n)
})
