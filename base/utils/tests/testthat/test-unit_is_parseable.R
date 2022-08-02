test_that("parseable unit", {
  expect_true(unit_is_parseable("miles"))
  expect_true(unit_is_parseable("   K   "))
  expect_true(unit_is_parseable("10cm"))
  expect_true(unit_is_parseable("m/s"))
  expect_true(unit_is_parseable("kg"))
})

test_that("Non-paresable unit", {
  expect_false(unit_is_parseable("fake"))
  expect_false(unit_is_parseable("gk"))
  expect_false(unit_is_parseable(NULL))
  expect_false(unit_is_parseable("Not parseable"))
  expect_false(unit_is_parseable("Loading"))
})

# This differ from `udunits2::ud.is.parseable`
#test_that("incompatiable unit to parse", {
#  expect_error(unit_is_parseable(""))
#})
