test_that("`take.samples` returns mean when stat is NA", {
  summary = list(mean = 10, stat = NA)
  expect_equal(take.samples(summary = summary), summary$mean)  
})

test_that("`take.samples` returns a vector of length sample.size for given summary stats", {
  summary = list(mean = 10, stat = 10)
  sample.size = 10
  expect_equal(length(take.samples(summary = summary, sample.size = sample.size)), sample.size)

  # Testing for exact return values for a simple example
  test.sample <- take.samples(summary = data.frame(mean = 1, stat = 1),
                               sample.size = 2)
  expect_equal(test.sample, c(2.26295428488079, 0.673766639294351))
})