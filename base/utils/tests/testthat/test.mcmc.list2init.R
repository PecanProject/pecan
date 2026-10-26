test_that("matrix initial values follow the monitored row and column indices", {
  columns <- c("beta[2,2]", "beta[1,1]", "beta[2,1]", "beta[1,2]")
  chain <- coda::mcmc(rbind(rep(0, 4), c(22, 11, 21, 12)))
  colnames(chain) <- columns
  second <- chain + 100
  result <- mcmc.list2init(coda::mcmc.list(chain, second))

  expect_equal(result[[1]]$beta, matrix(c(11, 21, 12, 22), nrow = 2))
  expect_equal(result[[2]]$beta, matrix(c(111, 121, 112, 122), nrow = 2))
})

test_that("unmonitored matrix entries are left missing", {
  chain <- coda::mcmc(rbind(c(0, 0), c(12, 23)))
  colnames(chain) <- c("beta[1,2]", "beta[2,3]")
  result <- mcmc.list2init(coda::mcmc.list(chain))
  expected <- matrix(NA_real_, nrow = 2, ncol = 3)
  expected[1, 2] <- 12
  expected[2, 3] <- 23
  expect_equal(result[[1]]$beta, expected)
})

test_that("scalar and vector initial values still use the final sample", {
  chain <- coda::mcmc(rbind(c(0, 0, 0), c(7, 11, 12)))
  colnames(chain) <- c("alpha", "beta[1]", "beta[2]")
  result <- mcmc.list2init(coda::mcmc.list(chain))
  expect_equal(unname(result[[1]]$alpha), 7)
  expect_equal(unname(result[[1]]$beta), c(11, 12))
})
