test_that("`n_leap_day()` able to correctly return number of leap days between 2 specified dates", {

  # having leap days
  expect_equal(n_leap_day("2000-01-01", "2003-12-31"), 1)
  expect_equal(n_leap_day("2000-01-01", "2004-12-31"), 2)
  
  # no leap days
  expect_equal(n_leap_day("2001-01-01", "2003-12-31"), 0)
})