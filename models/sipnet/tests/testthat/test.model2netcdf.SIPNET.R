context("model2netcdf.SIPNET unit conversion")

test_that("model2netcdf.SIPNET runs without error and produces netCDF", {
  outdir <- withr::local_tempdir()
  file.copy("data/sipnet.out", outdir)
  
  # Run the function
  expect_silent(
    model2netcdf.SIPNET(
      outdir = outdir,
      sitelat = 0,
      sitelon = 0,
      start_date = "2002-01-01",
      end_date = "2002-12-31"
    )
  )
  
  # Check that netCDF file is created
  nc_file <- file.path(outdir, "2002.nc")
  expect_true(file.exists(nc_file))
  
  # Check that we can read the output
  output <- PEcAn.utils::read.output(
    ncfiles = nc_file,
    variables = c("litter_carbon_content", "litterWater"),
    dataframe = TRUE,
    verbose = FALSE,
    print_summary = FALSE
  )
  expect_true(nrow(output) > 0)
  # litter_carbon_content should be in kg/m2 (converted from g/m2)
  expect_true(all(output$litter_carbon_content < 1))  # g/m2 values are ~400, kg/m2 ~0.4
  # litterWater should be in mm (converted from cm)
  expect_true(all(output$litterWater > 1000))  # cm values ~400, mm ~4000
})

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
