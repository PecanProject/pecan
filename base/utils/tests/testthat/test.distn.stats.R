context("functions that calculate statistics from parameterized distributions")

test_that("distn.stats works for different distributions",{
  expect_equal(distn.stats("norm", 0, 1), c(0,1))
  expect_equal(distn.stats("norm", -1, 1), c(-1,1))
  expect_equal(distn.stats("beta", 2, 3), c(2/5, sqrt(6/(5^2 * 6))))
  expect_equal(distn.stats("exp", 2), c(1/2, 1/2))
  expect_equal(distn.stats("t", 2), c(0, Inf))
  expect_equal(distn.stats("lnorm", 2, 3), 
               c(exp(2 + 0.5*3^2), sqrt(exp(2*2 + 3^2)*(exp(3^2)-1))))
  expect_equal(distn.stats("weibull", 2,3), 
               c(3 * gamma(1+1/2), 3^2 * (gamma(1 + 2/2) - (gamma(1 + 1/2))^2)))
})

test_that("distn.table.stats works for different distributions",{
  data(prior.distns)
  distn.table.stats(prior.distns)
  new.distn.table <- data.frame(distn = c("norm", "exp"), parama = c(1,1), paramb = c(1,1))
  expect_equivalent(distn.table.stats(new.distn.table), 
                    data.frame(mean = c(1,1), sd = c(1,1)))
})