test_that("check.prior.v.mapost function is functional",{
  set.seed(0)
  prior <- list(distn = "norm", parama = 0, paramb = 1)
  expect_error(check.prior.v.mapost(prior,
                                    ma.post = rnorm(1000, 3, 1)))
  expect_message(check.prior.v.mapost(prior,0),
                 "prior and posterior are consistent") 
})
