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

  # Note: Deleted test of unit_is_parseable("") here.
  # It was FALSE with {units} < v1.0, TRUE after that... and the change turned
  # out not to make any practical difference in PEcAn, so why test it?
})
