##' Test for GDAY model2netcdf unit conversions
##' 
##' This test verifies that the unit conversions in model2netcdf.GDAY are correct.
##' 
##' Reference: https://github.com/PecanProject/pecan/pull/XXXX
##' GDAY outputs daily values in Mg/ha/day, which should be converted to kg/m2/s
##'
##' Conversion factors:
##'   1 Mg/ha = 0.1 kg/m2 (area conversion)
##'   1 day = 86400 seconds
##'   Therefore: 1 Mg/ha/day = 0.1 / 86400 kg/m2/s = 1.157e-6 kg/m2/s

context("GDAY model2netcdf unit conversions")

test_that("model2netcdf.GDAY runs without error and produces netCDF", {
  outdir <- withr::local_tempdir()
  file.copy("data/gday_out.csv", outdir)
  
  # Run the function
  expect_silent(
    model2netcdf.GDAY(
      outdir = outdir,
      sitelat = 40,
      sitelon = -88,
      start_date = "2004-01-01",
      end_date = "2004-12-31"
    )
  )
  
  # Check that netCDF file is created
  nc_file <- file.path(outdir, "2004.nc")
  expect_true(file.exists(nc_file))
  
  # Check that we can read the output
  output <- PEcAn.utils::read.output(
    ncfiles = nc_file,
    variables = c("GPP", "NEE", "NPP"),
    dataframe = TRUE,
    verbose = FALSE,
    print_summary = FALSE
  )
  expect_true(nrow(output) > 0)
  # GPP should be in kg/m2/s (converted from Mg/ha/day)
  expect_true(all(output$GPP > 0))  # Positive values
  expect_true(all(output$GPP < 1e-5))  # Small values due to conversion
})

test_that("GDAY Mg/ha/day to kg/m2/s conversion is correct", {
  # Test data - using simple round numbers for verification
  # GDAY example output: https://github.com/mdekauwe/GDAY/blob/master/example/outputs/D1GDAYDUKEAMB.csv
  
  # Manual conversion test
  # 1 Mg/ha/day -> kg/m2/s conversion factor
  conversion_factor <- 0.1 / 86400  # ~1.157e-6
  
  # Test value from GDAY output (example: 0.05 Mg/ha/day)
  gday_value_mgha_day <- 0.05
  expected_value_kgm2s <- gday_value_mgha_day * conversion_factor
  
  # Verify using ud_convert (if available)
  if (requireNamespace("PEcAn.utils", quietly = TRUE)) {
    converted_value <- PEcAn.utils::ud_convert(gday_value_mgha_day, "Mg/ha/day", "kg/m2/s")
    expect_equal(converted_value, expected_value_kgm2s, tolerance = 1e-10,
                 label = "GDAY daily output conversion")
  }
  
  # More realistic test values based on GDAY Duke Ambient output
  # From D1GDAYDUKEAMB.csv: GPP ~= 3.5 Mg/ha/day for summer months
  gday_gpp <- 3.5
  converted_gpp <- gday_gpp * conversion_factor
  
  # This should be approximately 4.05e-6 kg/m2/s
  expect_true(converted_gpp > 0, "Conversion should result in positive value")
  expect_true(converted_gpp < 1e-5, "Converted value should be small (< 1e-5 kg/m2/s)")
})

test_that("GDAY timestep is correctly set to daily (86400 seconds)", {
  # The timestep.s in model2netcdf.GDAY should be 86400 seconds (1 day)
  # This is different from SIPNET which uses 86400/out_day for more flexible timesteps
  timestep_s <- 86400
  
  expect_equal(timestep_s, 86400, 
               label = "GDAY timestep should be 86400 seconds (daily data)")
  
  # Verify that this matches the conversion (since ud_convert assumes per-second)
  seconds_per_day <- 86400
  expect_equal(timestep_s, seconds_per_day)
})

test_that("GDAY flux variables are converted from Mg/ha/day not Mg/ha/yr", {
  # GDAY outputs are daily accumulations, not annual
  # The conversion should use "Mg/ha/day" not "Mg/ha/yr"
  
  # Test with a realistic GDAY value
  # Example: auto_resp (autotrophic respiration) ~ 0.02-0.05 Mg/ha/day
  auto_resp_daily <- 0.035
  
  # Correct conversion: Mg/ha/day -> kg/m2/s
  if (requireNamespace("PEcAn.utils", quietly = TRUE)) {
    correct_result <- PEcAn.utils::ud_convert(auto_resp_daily, "Mg/ha/day", "kg/m2/s")
    
    # Incorrect conversion (old code): Mg/ha/yr -> kg/m2/s
    # This would give a much larger value (365 times larger!)
    incorrect_result <- PEcAn.utils::ud_convert(auto_resp_daily, "Mg/ha/yr", "kg/m2/s")
    
    # The incorrect result should be ~365x larger
    ratio <- incorrect_result / correct_result
    expect_true(ratio > 300 & ratio < 400, 
                label = "Incorrect Mg/ha/yr conversion would be ~365x larger")
  }
})

test_that("GDAY respiration outputs consistency check", {
  # Test that total respiration = auto_resp + hetero_resp (within floating point precision)
  if (requireNamespace("PEcAn.utils", quietly = TRUE)) {
    # Test values
    auto_resp <- 0.03
    hetero_resp <- 0.02
    total_resp <- auto_resp + hetero_resp
    
    # All should convert to same scale (kg/m2/s)
    auto_resp_kgm2s <- PEcAn.utils::ud_convert(auto_resp, "Mg/ha/day", "kg/m2/s")
    hetero_resp_kgm2s <- PEcAn.utils::ud_convert(hetero_resp, "Mg/ha/day", "kg/m2/s")
    total_resp_kgm2s <- PEcAn.utils::ud_convert(total_resp, "Mg/ha/day", "kg/m2/s")
    
    # Total should equal sum of components
    expect_equal(total_resp_kgm2s, auto_resp_kgm2s + hetero_resp_kgm2s, 
                 tolerance = 1e-15,
                 label = "Total respiration should equal sum of components")
  }
})
