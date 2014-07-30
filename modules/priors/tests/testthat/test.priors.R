test_that("pr.dens works",{

  ## pr.dens()
  expect_that(nrow(pr.dens('norm', 0, 1, n=10, alpha=0.5)),
              equals(1))
  expect_that(nrow(pr.dens('norm', 0, 10, n=10, alpha=0.5)),
              equals(1)) # function should set n=1 when alpha = 0.5 
  expect_that(nrow(pr.dens('norm', 0, 10, n=10, alpha=0.4)),
              equals(10))
  expect_that(sum(pr.dens('norm', 0, 10, n=10, alpha=0.4)$x),
              equals(0))
})
test_that("pr.samp works",{

  ## pr.samp()
  expect_that(length(pr.samp('norm', 0, 1, 2)),
              equals(2))
  expect_that(pr.samp('norm', 0, 1, 1) < 100,
              is_true())

})


test_that("create.density.df works on both stated distribution and samples", {
  prior.df <- create.density.df(distribution = list('norm',  0, 1), n = 1000)
  samp.df <- create.density.df(samps = qnorm(1:100/101), n = 1000)
  expect_equal(colnames(prior.df), colnames(samp.df))
  expect_equal(dim(prior.df), dim(samp.df))
  expect_equal(colnames(prior.df), c("x", "y"))
  expect_equal(nrow(prior.df), 1000)
})


test_that("get.quantiles.from.density works", {
  samp.df <- create.density.df(samps = qnorm(1:100/101), n = 1000)
  test.q <- get.quantiles.from.density(samp.df, quantiles = c(0.25, 0.5, 0.75))
  expect_is(test.q, "data.frame")
  expect_equal(signif(test.q$x, 3), c(-0.711, -0.00337, 0.705))
  expect_equal(signif(test.q$y, 3), c(0.304, 0.381, 0.305))
  expect_equal(dim(test.q), c(3,2))
})

test_that("plot.prior.density returns ggplot object",{
  expect_is(plot.prior.density(pr.dens('norm', 0, 1)), "ggplot")
})
