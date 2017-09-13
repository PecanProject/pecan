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

host <- list(name = "localhost")
code <- quote({
  x <- 5
  y <- 10
  out <- list(xx = seq_len(x), yy = seq_len(y) * 2)
  dput(out)
})
result <- remote.execute.R(code = code, host = host)

code2 <- c("x <- 10", "y <- 7", "out <- list(x = seq(x), y = seq(y))", "dput(out)")
result <- remote.execute.R(code = code2, host = host)

code3 <- "
  n <- 10
  x <- rnorm(n)
  y <- runif(n)
  df <- data.frame(norm = x, unif = y)
  dput(df)
"
result <- remote.execute.R(code = code3, host = host)