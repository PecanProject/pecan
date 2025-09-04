context("extract_soil_gssurgo")

test_that("extract_soil_gssurgo returns valid NetCDF files for valid US coordinates", {
  skip_on_cran()
  skip_on_ci()
  lat <- 40.1164
  lon <- -88.2434
  tmp_outdir <- withr::local_tempdir("gssurgo_test_")
  
  res <- extract_soil_gssurgo(
    outdir = tmp_outdir, 
    lat = lat,
    lon = lon,
    size = 2,
    grid_size = 3,
    grid_spacing = 100,
    depths = c(0.15, 0.30)
  )

  expect_false(is.null(res))
  
  expect_type(res, "list")
  expect_gt(length(res), 1)
  expect_true(all(names(res) == "path"))
  
  # Validate files exist
  file_paths <- unlist(res)
  expect_true(all(file.exists(file_paths)))
  
  # Validate NetCDF content
  if (requireNamespace("ncdf4", quietly = TRUE)) {
    expected_vars <- c("fraction_of_sand_in_soil", "fraction_of_silt_in_soil", 
                      "fraction_of_clay_in_soil", "soil_organic_carbon_stock")
    
    # Skip first ensemble member (first ensemble member always uses the reported values without sampling) 
    # and use subsequent members are simulated ensemble member with uncertainty
    nc <- ncdf4::nc_open(file_paths[2])
    on.exit(ncdf4::nc_close(nc), add = TRUE)
    
    # Check required variables exist
    for (var in expected_vars) {
      expect_true(var %in% names(nc$var))
    }
    # Validate data quality
    sand <- ncdf4::ncvar_get(nc, "fraction_of_sand_in_soil")
    silt <- ncdf4::ncvar_get(nc, "fraction_of_silt_in_soil")
    clay <- ncdf4::ncvar_get(nc, "fraction_of_clay_in_soil")
    soc <- ncdf4::ncvar_get(nc, "soil_organic_carbon_stock")
    
    expect_true(all(is.finite(sand)))
    expect_true(all(is.finite(soc)))
    expect_true(all(sand >= 0 & sand <= 1))
    expect_true(all(silt >= 0 & silt <= 1))
    expect_true(all(clay >= 0 & clay <= 1))
    expect_true(all(soc >= 0))
    
    # Soil texture fractions should sum to ~1
    texture_sum <- sand + silt + clay
    expect_true(all(abs(texture_sum - 1) < 0.01))
  }
})

test_that("extract_soil_gssurgo performance is reasonable", {
  skip_on_cran()
  skip_on_ci()
  tmp_outdir <- withr::local_tempdir("gssurgo_test_")
  
  start_time <- Sys.time()
  res <- extract_soil_gssurgo(
    outdir = tmp_outdir,
    lat = 40.1164,
    lon = -88.2434,
    size = 1,
    grid_size = 3,
    grid_spacing = 100,
    depths = c(0.15)
  )
  end_time <- Sys.time()
  exec_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  expect_lt(exec_time, 40)
})

test_that("extract_soil_gssurgo handles ensemble generation", {
  skip_on_cran()
  skip_on_ci()
  tmp_outdir <- withr::local_tempdir("gssurgo_test_")
  
  res <- extract_soil_gssurgo(
    outdir = tmp_outdir,
    lat = 40.1164,
    lon = -88.2434,
    size = 3,
    grid_size = 3,
    grid_spacing = 100,
    depths = c(0.15, 0.30)
  )
  
  expect_false(is.null(res))
  
  expect_type(res, "list")
  expect_equal(length(res), 4)
  
  file_paths <- unlist(res)
  expect_true(all(file.exists(file_paths)))
})