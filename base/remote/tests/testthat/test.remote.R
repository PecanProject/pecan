# Quick test of remote functions
library(PEcAn.remote)
library(testthat)

good_host <- list(name = "localhost")
bad_host <- list(name = 'bigbadwolf')
test_that("test_remote identifies good and bad hosts", {
  expect_true(test_remote(good_host))
  expect_error(test_remote(bad_host))
  expect_false(test_remote(bad_host, stderr = FALSE))
})

echo_string <- "Hello!"
out <- remote.execute.cmd(host = good_host, cmd = "echo", args = echo_string)

test_that("Basic remote execution works as expected", {
  expect_identical(out, echo_string)
})

code <- quote({
  x <- 10
  result <- seq_len(x)
  dput(result)
})

out2 <- remote.execute.R(code = code, host = good_host)
test_that("Remote execution of R code works", {
  expect_identical(out2, seq_len(10))
})
