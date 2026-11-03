library(testthat)

test_that("load_x_netcdf properly loads NetCDF and parses time", {
  skip_if_not_installed("ncdf4")
  
  # Create a temporary NetCDF file
  nc_file <- tempfile(fileext = ".nc")
  
  # Define dimensions
  # Time: seconds since 2000-01-01 00:00:00
  time_vals <- c(0, 3600, 7200) # 3 hours
  dim_time <- ncdf4::ncdim_def("time", "seconds since 2000-01-01 00:00:00", time_vals)
  
  # Define variables
  var_gpp <- ncdf4::ncvar_def("GPP", "kg m-2 s-1", dim_time, missval = NA_real_, prec = "double")
  var_nee <- ncdf4::ncvar_def("NEE", "kg m-2 s-1", dim_time, missval = NA_real_, prec = "double")
  
  # Create the file
  nc_new <- ncdf4::nc_create(nc_file, list(var_gpp, var_nee))
  
  # Put data
  ncdf4::ncvar_put(nc_new, var_gpp, c(1.1, 2.2, NA)) # One missing value
  ncdf4::ncvar_put(nc_new, var_nee, c(10, 20, 30))
  
  ncdf4::nc_close(nc_new)
  
  # Test loading
  format_list <- list(na.strings = c("-9999", "-9999.0"))
  res <- load_x_netcdf(nc_file, format = format_list, site = NULL, vars = c("GPP", "NEE"))
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 3)
  expect_equal(ncol(res), 3) # GPP, NEE, and posix/time (depending on branch version)
  
  # Check variable extraction
  expect_equal(res$GPP[1], 1.1)
  expect_equal(res$GPP[2], 2.2)
  expect_true(is.na(res$GPP[3])) # Check NA replacement
  
  expect_equal(res$NEE, c(10, 20, 30))
  
  # Check time parsing
  time_col <- if ("posix" %in% names(res)) "posix" else "time"
  expect_true(time_col %in% names(res))
  expect_s3_class(res[[time_col]], "POSIXct")
  
  # Check actual time values
  expected_time <- as.POSIXct(c("2000-01-01 00:00:00", "2000-01-01 01:00:00", "2000-01-01 02:00:00"), tz = "UTC")
  expect_equal(res[[time_col]], expected_time)
  
  # Cleanup
  unlink(nc_file)
})
