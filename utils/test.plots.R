#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
context("plotting utils")

test_that("add.prior.density returns ggplot object",{
  expect_is(add.prior.density(pr.dens('norm', 0, 1)), 'ggplot')
})

test_that("create.density.df works on both stated distribution and samples", {
  prior.df <- create.density.df(distribution = list('norm',  0, 1), n = 1000)
  samp.df <- create.density.df(samps = qnorm(1:100/101), n = 1000)
  expect_equal(colnames(prior.df), colnames(samp.df))
  expect_equal(dim(prior.df), dim(samp.df))
  expect_equal(colnames(prior.df), c("x", "y"))
  expect_equal(nrow(prior.df), 1000)
})


test_that("get.quantiles.from.density works", {
  samp.df <- create.density.df(samps = qnorm(1:100/101), n = 1000)
  test.q <- get.quantiles.from.density(samp.df, quantiles = c(0.25, 0.5, 0.75))
  expect_is(test.q, "data.frame")
  expect_equal(signif(test.q$x, 3), c(-0.711, -0.00337, 0.705))
  expect_equal(signif(test.q$y, 3), c(0.304, 0.381, 0.305))
  expect_equal(dim(test.q), c(3,2))
})

