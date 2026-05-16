test_that("calcSpatialCov.matrix returns correct exponential covariance", {
  d <- matrix(c(0, 1, 2,
                1, 0, 1,
                2, 1, 0), nrow = 3)
  psi <- 0.5
  tau <- 2.0

  result <- calcSpatialCov.matrix(d, psi = psi, tau = tau)

  expected <- tau * exp(-psi * d)
  expect_equal(result, expected)
})

test_that("calcSpatialCov.matrix returns a matrix of the same dimensions as d", {
  d <- matrix(runif(25), nrow = 5)
  result <- calcSpatialCov.matrix(d, psi = 1, tau = 1)
  expect_equal(dim(result), dim(d))
})

test_that("calcSpatialCov.matrix diagonal is tau when d has zeros on diagonal", {
  n <- 4
  d <- matrix(1, n, n)
  diag(d) <- 0
  tau <- 3.0
  result <- calcSpatialCov.matrix(d, psi = 1, tau = tau)
  expect_equal(diag(result), rep(tau, n))
})

test_that("calcSpatialCov.matrix is symmetric when d is symmetric", {
  d <- as.matrix(dist(1:5))
  result <- calcSpatialCov.matrix(d, psi = 0.3, tau = 1.5)
  expect_equal(result, t(result))
})

test_that("calcSpatialCov.list and calcSpatialCov.matrix agree on single-component case", {
  d <- as.matrix(dist(1:4))
  psi <- 0.4
  tau <- 2.0

  result_matrix <- calcSpatialCov.matrix(d, psi = psi, tau = tau)
  result_list   <- calcSpatialCov.list(list(d), psi = psi, tau = tau)

  expect_equal(result_matrix, result_list)
})
