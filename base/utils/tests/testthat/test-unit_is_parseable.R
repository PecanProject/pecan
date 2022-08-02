test_that("parseable unit", {
  expect_equal(unit_is_parseable("miles"), TRUE)
  expect_equal(unit_is_parseable("   K   "), TRUE)
  expect_equal(unit_is_parseable("10cm"), TRUE)
  expect_equal(unit_is_parseable("m/s"), TRUE)
  expect_equal(unit_is_parseable("kg"), TRUE)
})

test_that("Non-paresable unit", {
  expect_equal(unit_is_parseable("fake"), FALSE)
  expect_equal(unit_is_parseable("gk"), FALSE)
  expect_equal(unit_is_parseable(NULL), FALSE)
  expect_equal(unit_is_parseable("Not parseable"), FALSE)
  expect_equal(unit_is_parseable("Loading"), FALSE)
})

# This differ from `udunits2::ud.is.parseable`
#test_that("incompatiable unit to parse", {
#  expect_error(unit_is_parseable(""))
#})
