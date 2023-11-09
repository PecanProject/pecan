test_that("`r2bugs.distributions()` able to convert R parameterization to BUGS parameterization", {
  priors <- data.frame(distn = c('weibull', 'lnorm', 'norm', 'gamma'),
                      parama = c(1, 1, 1, 1),
                      paramb = c(2, 2, 2, 2))
  res <- r2bugs.distributions(priors)
  expect_equal(res$distn, c("weib", "lnorm", "norm", "gamma"))
  expect_equal(res$parama, c(1, 1, 1, 1))
  expect_equal(res$paramb, c(0.50, 0.25, 0.25, 2.00))
})

test_that("`bugs2r.distributions()` able to convert BUGS parameterization to R parameterization", {
  priors <- data.frame(distn = c('weib', 'lnorm', 'norm', 'gamma'),
                      parama = c(1, 1, 1, 1),
                      paramb = c(0.50, 0.25, 0.25, 2.00))
  res <- bugs2r.distributions(priors)
  expect_equal(res$distn, c("weibull", "lnorm", "norm", "gamma"))
  expect_equal(res$parama, c(1, 1, 1, 1))
  expect_equal(res$paramb, c(2, 2, 2, 2))
})