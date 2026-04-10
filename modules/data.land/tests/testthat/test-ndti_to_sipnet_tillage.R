context("ndti_to_sipnet_tillage")

test_that("returns a numeric vector of correct length", {
  out <- ndti_to_sipnet_tillage(c(0.2, 0.5, 0.8))
  expect_type(out, "double")
  expect_length(out, 3)
})

test_that("default slope clamps correctly at boundaries", {
  out <- ndti_to_sipnet_tillage(c(0.00, 0.30, 0.70, 1.00))
  expect_equal(out, c(0, 0, 1, 1))
})

test_that("midpoint maps to 0.5 with default parameters", {
  out <- ndti_to_sipnet_tillage(0.50)
  expect_equal(out, 0.5, tolerance = 1e-9)
})

test_that("custom slope scales output correctly", {
  # slope = 1 means ramp reaches 1 at delta_ndti = 1.30
  out <- ndti_to_sipnet_tillage(c(0.30, 0.80), slope = 1.0)
  expect_equal(out[1], 0)
  expect_equal(out[2], 0.5, tolerance = 1e-9)
})

test_that("output is always clamped to [0, 1]", {
  out <- ndti_to_sipnet_tillage(seq(0, 1, length.out = 50))
  expect_true(all(out >= 0))
  expect_true(all(out <= 1))
})

test_that("NA propagates and negative delta_ndti clamps to zero", {
  out <- ndti_to_sipnet_tillage(c(-0.1, NA, 0.70))
  expect_equal(out[1], 0)
  expect_true(is.na(out[2]))
  expect_equal(out[3], 1)
})

test_that("invalid inputs produce informative errors", {
  expect_error(
    ndti_to_sipnet_tillage(0.5, no_till_threshold = "a"),
    "single numeric"
  )
  expect_error(
    ndti_to_sipnet_tillage(0.5, slope = -1),
    "non-negative"
  )
})