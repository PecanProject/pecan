#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
context("test approx.posterior function")

data("trait.mcmc", package = "PEcAn.utils")
data("prior.distns", package = "PEcAn.utils")

test_that("test data are as expected", {
  expect_equal(names(trait.mcmc),
               c("SLA", "leaf_width", "Vcmax", "fineroot2leaf", "root_respiration_rate",
                 "root_turnover_rate", "stomatal_slope"))
  expect_is(trait.mcmc, "list")
  expect_is(trait.mcmc[[1]], "mcmc.list")
  expect_is(prior.distns, "data.frame")
  expect_equal(dim(prior.distns), c(15, 4))
  expect_is(prior.distns$distn, "character")
  expect_is(prior.distns$parama, "numeric")
  expect_is(prior.distns$paramb, "numeric")
})

test_that("approx.posterior function returns expected dataframe given test data", {
  test.ap <- approx.posterior(trait.mcmc, priors = prior.distns)
  expect_is(test.ap, "data.frame")
  expect_equal(dim(test.ap), c(15, 4))
  expect_equal(dim(test.ap), dim(prior.distns))
  expect_equal(colnames(test.ap), colnames(prior.distns))
  expect_equal(rownames(test.ap), rownames(prior.distns))
  expect_equal(test.ap$n, prior.distns$n)
})
