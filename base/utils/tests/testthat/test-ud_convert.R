test_that("unit conversions", {
  expect_equal(ud_convert(4, "in", "cm"), 10.16)
  expect_equal(ud_convert(23, "degC", "K"), 296.15)
  expect_equal(ud_convert(6, "days", "seconds"), 518400)
  expect_equal(ud_convert(32, "hour", "seconds"), 115200)
  expect_equal(ud_convert(15, "days", "hour"), 360)
  expect_equal(ud_convert(-7, "ppm", "mol/mol"), -7e-06)
})

test_that("unit conversion invariants", {
  expect_equal(ud_convert(1, "g", "g"), 1) 
  expect_equal(ud_convert(0, "g", "kg"), 0)
  expect_equal(ud_convert(Inf, "g", "kg"), Inf)
})

test_that("incompatible units", {
  expect_error(ud_convert(1, "miles", "grams"))
  expect_error(ud_convert(1, "radians", "degC"))
  expect_error(ud_convert(1, "in", "grams"))
})

test_that("output is type numeric and not class \"units\"", {
  x <- ud_convert(23, "degC", "K")
  testthat::expect_failure(expect_s3_class(x, "units"))
  testthat::expect_type(x, "double")

})

test_that("ud_convert() handles difftime", {
  x <- ud_convert(as.difftime("12:00:00"), u1 = "hours", u2 = "days")
  expect_is(x, "numeric")
  expect_equal(x, 0.5)
})

test_that("ud_convert() warns with wrong input units for difftime", {
  expect_warning(ud_convert(as.difftime("12:00:00"), u1 = "years", u2 = "minutes"))
  #should still error if units are not convertible
  expect_error(ud_convert(as.difftime("12:00:00"), u1 = "kilograms", u2 = "minutes"))
})