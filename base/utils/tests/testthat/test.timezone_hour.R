test_that("`timezone_hour()` able to correctly return number of hours offset to UTC for a timezone", {
  expect_equal(timezone_hour('US/Pacific'), -8)
  expect_equal(timezone_hour('US/Eastern'), -5)

  # for numeric
  expect_equal(timezone_hour(-8), -8)
})