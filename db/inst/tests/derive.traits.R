test_that("take.samples works";{
  expect_equal(take.samples(trait = data.frame(mean = 1, stat = NA)), 1)
  set.seed(0)
  test.sample <- take.samples(trait = data.frame(mean = 1, stat = 1),
                               sample.size = 2)
  expect_equal(test.sample, c(2.26295428488079, 0.673766639294351))
})
