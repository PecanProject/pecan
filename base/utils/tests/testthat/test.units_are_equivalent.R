test_that("`units_are_equivalent()` able to identify if the units are equivalent or not", {
  # Equivalent units
  expect_true(units_are_equivalent("m/s", "m s-1"))
  # Non-equivalent units
  expect_error(units_are_equivalent("m/s", "m s-2"))
})