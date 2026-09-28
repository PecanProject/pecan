

test_that("sipnet2datetime - standard vectorised input", {
  years <- c(2023, 2023)
  doys <- c(1, 32)
  hours <- c(0, 10.5)

  datetimes <- sipnet2datetime(years, doys, hours)

  expect_equal(length(datetimes), 2)
  expect_equal(datetimes[1], as.POSIXct("2023-01-01 00:00:00", tz = "UTC"))
  expect_equal(datetimes[2], as.POSIXct("2023-02-01 10:30:00", tz = "UTC"))
  }
)

test_that("sipnet2datetime - leap years",{

  expect_equal(
    format(sipnet2datetime(2024, 60, 0), "%Y-%m-%d"), "2024-02-29")

  expect_equal(
    format(sipnet2datetime(2023, 60, 0), "%Y-%m-%d"), "2023-03-01")
  }
)

test_that("sipnet2datetime - decimal accuracy", {
  expect_equal(format(sipnet2datetime(2023, 1, 13.75), "%H:%M:%S"),
               "13:45:00")

  expect_equal(format(sipnet2datetime(2023, 1, 23.9999), "%Y-%m-%d %H:%M:%S"),
               "2023-01-01 23:59:59")

  }
)

test_that("sipnet2datetime - UTC timezone", {
  expect_equal(attr(sipnet2datetime(2023, 1, 1), "tzone"), "UTC")
  }
)


test_that("sipnet-datetime round trip", {
  t <- sipnet2datetime(2016, 3, 4.5)
  expect_equal(t, as.POSIXct("2016-01-03 04:30:00 UTC", tz="UTC"))
  expect_equal(datetime2sipnet(t), list(year = 2016, yday = 3, hour = 4.5))
})
