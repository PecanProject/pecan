
test_that("spline.truncate truncates a vector at zero iff only values < min.quantile are negative",{
  set.seed(0)
  x <- c(rgamma(9980,1,1),
         rnorm(20))
  x.t <- spline.truncate(x, pnorm(-3))
  expect_true(min(x) < 0)
  expect_true(min(x.t) >= 0)
  expect_true(length(x) == length(spline.truncate(x)))
  ## test that it does not truncate a vector with < 0 at  min.quantile
  x.tt <- spline.truncate(x, min.quantile =  sum(x<0)/length(x))
  expect_true(min(x.tt) < 0)
}
