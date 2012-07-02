test_that("p.point.in.prior function is functional",{
  prior <- list(distn = "norm", parama = 0, paramb = 1)
  expect_equal(p.point.in.prior(point = 3, prior = prior),
               pnorm(3))
  expect_equal(p.point.in.prior(point = -3, prior = prior),
               pnorm(-3))
})
