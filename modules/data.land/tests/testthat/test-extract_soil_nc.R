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
    radius = 500,
    depths = c(0, 0.15, 0.30)
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
    radius = 500,
    depths = c(0, 0.15)
  )
  end_time <- Sys.time()
  exec_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  expect_lt(exec_time, 60)
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
    radius = 500,
    depths = c(0, 0.15, 0.30)
  )
  
  expect_false(is.null(res))
  
  expect_type(res, "list")
  expect_equal(length(res), 5)
  file_paths <- unlist(res)
  expect_true(all(file.exists(file_paths)))
})

test_that("extract_soil_gssurgo works with custom AOI polygon", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")
  
  tmp_outdir <- withr::local_tempdir("gssurgo_test_")
  
  # Create small polygon AOI
  aoi_coords <- matrix(c(
    -88.25, 40.11,
    -88.24, 40.11,
    -88.24, 40.12,
    -88.25, 40.12,
    -88.25, 40.11
  ), ncol = 2, byrow = TRUE)
  
  aoi <- terra::vect(aoi_coords, type = "polygons", crs = "epsg:4326")
  
  res <- extract_soil_gssurgo(
    outdir = tmp_outdir,
    aoi = aoi,
    size = 2,
    depths = c(0, 0.15, 0.30)
  )
  
  expect_false(is.null(res))
  expect_type(res, "list")
  expect_gt(length(res), 0)
  
  file_paths <- unlist(res)
  expect_true(all(file.exists(file_paths)))
})

test_that("extract_soil_gssurgo handles different buffer radii", {
  skip_on_cran()
  skip_on_ci()
  tmp_outdir <- withr::local_tempdir("gssurgo_test_")
  
  # Small radius
  res_small <- extract_soil_gssurgo(
    outdir = tmp_outdir,
    lat = 40.1164,
    lon = -88.2434,
    size = 1,
    radius = 200,
    depths = c(0, 0.15)
  )
  
  # Larger radius (should potentially capture more mukeys)
  res_large <- extract_soil_gssurgo(
    outdir = tmp_outdir,
    lat = 40.1164,
    lon = -88.2434,
    size = 1,
    radius = 1000,
    depths = c(0, 0.15)
  )
  
  expect_type(res_small, "list")
  expect_type(res_large, "list")
})

test_that("extract_soil_gssurgo generates distinct ensemble members from Dirichlet sampling", {
  # This test verifies that the Dirichlet-based texture sampling produces 

  # variability across ensemble members, reflecting uncertainty in soil properties.
  # Different ensemble files should have different texture values (not identical).
  skip_on_cran()
  skip_on_ci()
  tmp_outdir <- withr::local_tempdir("gssurgo_test_")
  
  res <- extract_soil_gssurgo(
    outdir = tmp_outdir,
    lat = 40.1164,
    lon = -88.2434,
    size = 5,  # Multiple ensemble members to check variability
    radius = 500,
    depths = c(0, 0.15, 0.30)
  )
  
  expect_false(is.null(res))
  expect_gt(length(res), 2)
  
  if (requireNamespace("ncdf4", quietly = TRUE) && length(res) >= 3) {
    # Compare two different ensemble members (skip first - it's unsampled)
    nc1 <- ncdf4::nc_open(unlist(res)[2])
    nc2 <- ncdf4::nc_open(unlist(res)[3])
    on.exit({
      ncdf4::nc_close(nc1)
      ncdf4::nc_close(nc2)
    }, add = TRUE)
    
    sand1 <- ncdf4::ncvar_get(nc1, "fraction_of_sand_in_soil")
    sand2 <- ncdf4::ncvar_get(nc2, "fraction_of_sand_in_soil")
    
    # Ensemble members should show variability (not identical)
    expect_false(all(sand1 == sand2))
  }
})

test_that("extract_soil_gssurgo requires depths to start with 0", {
  expect_error(
    extract_soil_gssurgo(
      outdir = withr::local_tempdir(),
      lat = 40,
      lon = -88,
      depths = c(0.15, 0.30)  # Missing 0 at start
    ),
    regexp = "First depth must be 0"
  )
})

test_that("gssurgo_fetch_area returns raw soil data for inspection", {
  skip_on_cran()
  skip_on_ci()
  
  result <- gssurgo_fetch_area(
    lat = 40.1164,
    lon = -88.2434,
    radius = 500,
    depths = c(0, 0.15, 0.30)
  )
  
  expect_type(result, "list")
  expect_true("soilprop" %in% names(result))
  expect_true("mukey_counts" %in% names(result))
  expect_true("depths_cm" %in% names(result))
  
  # Validate raw data structure
  expect_s3_class(result$soilprop, "data.frame")
  expect_true(all(c("sandtotal_r", "silttotal_r", "claytotal_r", 
                    "mukey", "cokey") %in% names(result$soilprop)))
  
  # Values should be in original units (percentages, not fractions)
  expect_true(all(result$soilprop$sandtotal_r <= 100, na.rm = TRUE))
})
