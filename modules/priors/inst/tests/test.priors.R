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
