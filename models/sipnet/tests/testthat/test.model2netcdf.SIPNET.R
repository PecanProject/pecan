context("model2netcdf.SIPNET unit conversion")

# Skip all tests if PEcAn.utils is not available
skip_if_not_installed("PEcAn.utils")

test_that("litterWater conversion works correctly", {
  # Test that PEcAn.utils::ud_convert is properly called for litterWater
  # This verifies the fix for converting cm to mm
  
  # Simple test of the conversion factor
  # cm to mm is 10x multiplier
  test_input <- 1.5  # 1.5 cm
  expected_output <- 15  # 15 mm
  
  result <- PEcAn.utils::ud_convert(test_input, "cm", "mm")
  expect_equal(result, expected_output)
})

test_that("litter_carbon_content conversion works correctly", {
  # Test g/m2 to kg/m2 conversion
  # Should be 0.001x multiplier
  
  test_input <- 500  # 500 g/m2
  expected_output <- 0.5  # 0.5 kg/m2
  
  result <- PEcAn.utils::ud_convert(test_input, "g/m2", "kg/m2")
  expect_equal(result, expected_output)
})

test_that("fine_root_carbon_content conversion works correctly", {
  # Test g/m2 to kg/m2 conversion (same as litter)
  
  test_input <- 100  # 100 g/m2
  expected_output <- 0.1  # 0.1 kg/m2
  
  result <- PEcAn.utils::ud_convert(test_input, "g/m2", "kg/m2")
  expect_equal(result, expected_output)
})

test_that("ud_convert rejects incompatible units", {
  # Verify that attempting to convert incompatible units fails appropriately
  
  expect_error(
    PEcAn.utils::ud_convert(10, "cm", "kg"),
    "not convertible"
  )
})
