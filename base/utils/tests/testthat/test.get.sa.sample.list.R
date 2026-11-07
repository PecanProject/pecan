test_that("get.sa.sample.list returns quantiles for each PFT and no env", {
  res <- get.sa.sample.list(list(a = list(x = 1:101), b = list(y = 0:100)), c(0.25, 0.5))

  expect_named(res, c("a", "b"))
  expect_equal(res$a["50", "x"], 51)
  expect_equal(res$b["25", "y"], 25)
})
