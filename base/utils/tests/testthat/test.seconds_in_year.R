test_that("`seconds_in_year()` able to return number of seconds in a given year(also for a vector of years)", {
  # leap year
  expect_equal(seconds_in_year(2000), 31622400)
  # non leap year
  expect_equal(seconds_in_year(2001), 31536000)
  # vector of years
  expect_equal(seconds_in_year(2000:2004), c(31622400, 31536000, 31536000, 31536000, 31622400))
})