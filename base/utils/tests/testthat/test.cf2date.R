test_that("`cf2datetime()` able to convert CF-style date-time to POSIXct date-time along with taking care of leap years", {
  expect_equal(cf2datetime(5, "days since 1981-01-01"), as.POSIXct("1981-01-06", tz = "UTC"))
  expect_equal(cf2datetime(27, "minutes since 1963-01-03 12:00:00 -05:00"), as.POSIXct("1963-01-03 17:27:00", tz = "UTC"))
  # nom-leap year
  expect_equal(cf2datetime(365, "days since 1999-01-01"), as.POSIXct("2000-01-01", tz = "UTC"))
  # leap year
  expect_equal(cf2datetime(365, "days since 2000-01-01 12:00:00 -05:00"), as.POSIXct("2000-12-31 17:00:00", tz = "UTC"))
})