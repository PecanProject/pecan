##' Test for GDAY model2netcdf unit conversions
##' 
##' This test verifies that the unit conversions in model2netcdf.GDAY are correct.
##' 
##' Reference: https://github.com/PecanProject/pecan/pull/3719
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
    variables = c("GPP", "AbvGrndWood"),
    dataframe = TRUE,
    verbose = FALSE,
    print_summary = FALSE
  )
  # GPP should be in kg/m2/s (converted from Mg/ha/day)
  secs_day <- 86400
  kg_Mg <- 1000
  m2_ha <- 10000
  gday2pecan <- kg_Mg/m2_ha/secs_day
  expect_equal(nrow(output), 5)
  expect_equal(output$GPP, rep(0.5, 5) * gday2pecan, tolerance = 1e-6)
  
  # AbvGrndWood is a stock (Mg/ha), not a flux (Mg/ha/day).
  # Conversion: 1 Mg/ha = 0.1 kg/m2
  stock_conv <- 0.1
  expect_equal(output$AbvGrndWood, rep(150, 5) * stock_conv, tolerance = 1e-6)
})
