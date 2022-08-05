test_that("parseable unit", {
  expect_true(unit_is_parseable("miles"))
  expect_true(unit_is_parseable("   K   "))
  expect_true(unit_is_parseable("10cm"))
  expect_true(unit_is_parseable("m/s"))
  expect_true(unit_is_parseable("kg"))
})

test_that("Non-parseable unit", {
  expect_false(unit_is_parseable("fake"))
  expect_false(unit_is_parseable("kg / fake"))
  expect_false(unit_is_parseable(NULL))

  # Note: This behavior differs from `udunits2::ud.is.parseable("")`
  #  (which returns TRUE), but is better aligned with PEcAn's usage
  #  of "parseable" to mean "will work when passed to ud_convert".
  # Since ud_convert does not support any useful conversions of "",
  #  we report it as unparseable.
  expect_false(unit_is_parseable(""))
})

