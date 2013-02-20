
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
})

test_that("sensitivity and elasticity calculations are done at the median",{
  expect_equal(get.elasticity(1, c(1, 10, 100), c(1, 0.1, 0.01)), 10/0.1)
  testfun <- sa.splinefun(c(1,2,3), c(1, 4, 6)) ## y = x2; first derivative = 2
  expect_equal(get.sensitivity(c(1,10, 100), testfun), 2)
  expect_equal(signif(get.coef.var(c(1, 10, 100)), 4), 5.474)
})