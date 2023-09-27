test_that("`cf2datetime()` able to convert CF-style date-time to POSIXct date-time along with taking care of leap years", {
  expect_equal(cf2datetime(5, "days since 1981-01-01"), as.POSIXct("1981-01-06", tz = "UTC"))
  expect_equal(cf2datetime(27, "minutes since 1963-01-03 12:00:00 -05:00"), as.POSIXct("1963-01-03 17:27:00", tz = "UTC"))
  # nom-leap year
  expect_equal(cf2datetime(365, "days since 1999-01-01"), as.POSIXct("2000-01-01", tz = "UTC"))
  # leap year
  expect_equal(cf2datetime(365, "days since 2000-01-01 12:00:00 -05:00"), as.POSIXct("2000-12-31 17:00:00", tz = "UTC"))
})

test_that("`datetime2cf()` able to convert POSIXct date-time to CF-style date-time", {
  expect_equal(datetime2cf("1990-10-05", "days since 1990-01-01", tz = "UTC"), 277)
  expect_equal(datetime2cf("1963-01-03 17:27:00", "minutes since 1963-01-03 12:00:00 -05:00", tz = "UTC"), 27)
})

test_that("`datetime2doy()` and `cf2doy()` able to extract Julian day from POSIXct or CF date-times respectively(cf2doy internally converts CF to POSIXct and calls datetime2doy)", {
  
  # POSIXct date-times
  expect_equal(datetime2doy("2010-01-01"), 1)
  expect_equal(datetime2doy("2010-01-01 12:00:00"), 1.5)

  # CF date-times
  expect_equal(cf2doy(0, "days since 2007-01-01"), 1)
})