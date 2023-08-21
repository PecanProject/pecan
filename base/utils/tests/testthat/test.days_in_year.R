test_that("`days_in_year()` correctly returns number of days when provided a year or a vector of years", {
  expect_equal(days_in_year(2010), 365)
  expect_equal(days_in_year(2012), 366)
  expect_equal(days_in_year(2010:2012), c(365, 365, 366))
})