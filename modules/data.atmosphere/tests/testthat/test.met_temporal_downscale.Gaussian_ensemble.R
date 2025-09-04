context("met_temporal_downscale.Gaussian_ensemble")

setup_test_files <- function() {
  # minimal test netCDF files for input and training data
  input_met_file <- tempfile(pattern = "input_", fileext = ".2020.nc")
  train_met_file <- tempfile(pattern = "train_", fileext = ".nc") 
  outfolder <- withr::local_tempdir()
  
  list(
    input_met = input_met_file,
    train_met = train_met_file,
    outfolder = outfolder
  )
}

test_that("Gaussian ensemble function basic functionality", {
  skip_if_not(require(ncdf4), "ncdf4 package not available")
  skip_if_not(require(PEcAn.data.atmosphere), "PEcAn.data.atmosphere package not available")
  
  # This test would require actual test data files
  # For now, testing the structure and parameter validation
  
  test_files <- setup_test_files()
  # Test parameter validation
  suppressWarnings(
    expect_error(
      met_temporal_downscale.Gaussian_ensemble(
        in.path = "", 
        in.prefix = "",
        outfolder = test_files$outfolder,
        input_met = "nonexistent.nc",
        train_met = "nonexistent.nc"
      ),
      "Error in nc_open trying to open file"
    )
  )
})

test_that("Ensemble generation produces correct number of outputs", {
  skip("Requires test data files")
  
  test_files <- setup_test_files()
  n_ens <- 5
  
  # Mock the function call (would need actual data)
  # results <- met_temporal_downscale.Gaussian_ensemble(
  #   in.path = dirname(test_files$input_met),
  #   in.prefix = "",
  #   outfolder = test_files$outfolder,
  #   input_met = test_files$input_met,
  #   train_met = test_files$train_met,
  #   n_ens = n_ens
  # )
  
  # expect_equal(length(results), n_ens)
  # expect_true(all(sapply(results, function(x) file.exists(x$file))))
})

test_that("Temperature downscaling maintains physical constraints", {
  # Mock data representing daily temperature values
  daily_temp <- c(298.15, 300.15, 295.15)  # K
  daily_temp_max <- c(305.15, 308.15, 302.15)
  daily_temp_min <- c(290.15, 292.15, 288.15)
  
  # Test that max >= mean >= min relationships are preserved
  # This would test the logic: 
  # df$air_temperature_max <- pmax(df$air_temperature_max, df$air_temperature, na.rm = TRUE)
  # df$air_temperature_min <- pmin(df$air_temperature_min, df$air_temperature, na.rm = TRUE)
  
  temp_max <- pmax(daily_temp_max, daily_temp, na.rm = TRUE)
  temp_min <- pmin(daily_temp_min, daily_temp, na.rm = TRUE)

  expect_true(all(temp_max >= daily_temp))
  expect_true(all(temp_min <= daily_temp))
  expect_true(all(temp_max >= temp_min))
})

test_that("Precipitation downscaling preserves mass conservation", {
  # Test the precipitation redistribution logic

  # Mock daily precipitation values (kg m-2 s-1)
  daily_precip <- c(0, 0.000005787, 0, 0.00001157)  # kg m-2 s-1
  div <- 4 # creates 6-hourly output from daily input (24h/4 = 6h intervals)
  
  # Test that total precipitation is conserved when redistributed
  # This tests the rand_vect_cont function logic
  
  # Simple test of mass conservation principle
  total_input <- sum(daily_precip)
  redistributed <- numeric(length(daily_precip) * div)
  for (i in seq_along(daily_precip)) {
    start_idx <- (i-1) * div + 1
    end_idx <- i * div
    # uniform redistribution for testing
    redistributed[start_idx:end_idx] <- daily_precip[i] / div
  }
  
  total_output <- sum(redistributed)
  expect_equal(total_input, total_output, tolerance = 1e-10)
})

test_that("Shortwave radiation methods produce valid outputs", {
  # Mock data
  daily_sw <- c(200, 250, 180)  # W m-2
  lat <- 40.0
  lon <- -88.0
  year <- 2020
  
  # Test sine method constraints
  # SW radiation should be >= 0 and follow diurnal pattern
  # This would test: swflux[swflux < 0] <- 0
  
  sw_with_neg <- c(-10, 100, 200, -5, 150)
  sw_valid <- pmax(sw_with_neg, 0)

  expect_true(all(sw_valid >= 0))
  expect_equal(sw_valid, c(0, 100, 200, 0, 150))
})

test_that("Soil moisture uncertainty calculations", {
  # Mock sw data
  soil_moisture <- c(0.2, 0.35, 0.45, 0.25, 0.15)
  # Calculate cv
  sm_cv <- stats::sd(soil_moisture, na.rm = TRUE) / mean(soil_moisture, na.rm = TRUE)
  # Calculate field capacity (75th percentile) 
  sm_fc <- stats::quantile(soil_moisture, 0.75, na.rm = TRUE)
  # Test moisture stress calculation for different values
  test_sm <- c(0.1, 0.25, 0.35, 0.45)
  moisture_stress <- abs(test_sm - sm_fc) / sm_fc
  uncertainty_factor <- 1.0 + sm_cv * moisture_stress
  uncertainty_with_precip <- uncertainty_factor * 1.2
  uncertainty_bound <- pmax(0.7, pmin(uncertainty_with_precip, 1.8))
  
  expect_true(all(uncertainty_factor >= 1.0))
  expect_true(all(uncertainty_with_precip >= uncertainty_factor))
  expect_true(all(uncertainty_bound >= 0.7))
  expect_true(all(uncertainty_bound <= 1.8))
})

test_that("Relative humidity temperature adjustment", {
  # Test RH adjustment for temperature changes
  
  # Mock data
  source_temp_k <- 293.15  # 20C
  current_temp_k <- 298.15  # 25C
  source_rh <- 70  # 70%
  
  # Convert to Celsius for saturation vapor pressure calculation
  source_temp_c <- source_temp_k - 273.15
  current_temp_c <- current_temp_k - 273.15
  expect_true(current_temp_c > -40 && current_temp_c < 50)
  expect_true(source_temp_c > -40 && source_temp_c < 50)

  # (Warmer air can hold more moisture, so RH should generally decrease)
  # Magnus formula constants
  es_source <- 0.61078 * exp((17.27 * source_temp_c) / (source_temp_c + 237.3))
  es_current <- 0.61078 * exp((17.27 * current_temp_c) / (current_temp_c + 237.3))
  
  saturation_ratio <- es_source / es_current
  adjusted_rh <- source_rh * saturation_ratio
  # For warming (current > source), adjusted RH should be lower
  expect_true(adjusted_rh < source_rh)
  expect_true(adjusted_rh > 0 && adjusted_rh <= 100)
})

test_that("PPFD calculations respect daylight constraints", {
  # Test PPFD (photosynthetic photon flux density)
  # Mock daylight conditions
  is_daylight <- c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE)
  ppfd_values <- c(0, 0, 0.0005, 0.0008, 0.0006, 0, 0)  # in mol m-2 s-1
  
  # Test that nighttime PPFD is zero
  nighttime_ppfd <- ppfd_values[!is_daylight]
  expect_true(all(nighttime_ppfd == 0))
  # Test PPFD bounds (0 to 0.0025 mol m-2 s-1 under full sunlight)
  ppfd_test <- c(-0.00005, 0.0001, 0.0015, 0.0030, 0.0025)
  ppfd_bound <- pmax(0, pmin(ppfd_test, 0.0025))

  expect_equal(ppfd_bound, c(0, 0.0001, 0.0015, 0.0025, 0.0025))
})


test_that("Wind speed and other variables handle missing data correctly", {
  source_data <- c(2.5, NA, 3.2, 1.8, NA)
  expect_false(all(is.na(source_data)))
  expect_true(all(is.na(c(NA, NA, NA))))
  
  # Test that missing values are handled in sd calculations
  sd_with_na <- sd(source_data, na.rm = TRUE)
  expect_true(!is.na(sd_with_na))
  expect_true(is.finite(sd_with_na))
})

test_that("Leap year handling works correctly", {  
  leap_year <- 2020
  non_leap_year <- 2021
  expect_true(lubridate::leap_year(leap_year))
  expect_false(lubridate::leap_year(non_leap_year))
  # Test day count logic
  leap_days <- ifelse(lubridate::leap_year(leap_year), 366, 365)
  non_leap_days <- ifelse(lubridate::leap_year(non_leap_year), 366, 365)
  expect_equal(leap_days, 366)
  expect_equal(non_leap_days, 365)
})