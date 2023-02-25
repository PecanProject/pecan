context("Quick test of remote functions")
library(PEcAn.remote)
library(testthat)

good_host <- list(name = "localhost")
bad_host <- list(name = 'bigbadwolf')
test_that("test_remote identifies good and bad hosts", {
  expect_true(test_remote(good_host))
  expect_error(test_remote(bad_host))
  expect_false(test_remote(bad_host, stderr = FALSE))
})
