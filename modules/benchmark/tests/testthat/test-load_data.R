library(testthat)

test_that("load_data correctly coordinates data loading and unit conversion", {
  
  # 1. Setup mock CSV data (AmeriFlux style format)
  csv_file <- tempfile(fileext = ".csv")
  df_input <- data.frame(
    TIMESTAMP_START = c("200001010000", "200001010030"),
    temp_c    = c(20.0, 21.0) # Celsius
  )
  write.csv(df_input, csv_file, row.names = FALSE)
  
  # 2. Setup format config
  vars_df <- data.frame(
    variable_id = c(1, 2),
    input_name = c("TIMESTAMP_START", "temp_c"),
    input_units = c("NA", "Celsius"),
    pecan_name = c("time", "AirT"),
    pecan_units = c("NA", "K"),
    bety_name = c("time", "AirT"),
    storage_type = c("%Y%m%d%H%M", "numeric"),
    stringsAsFactors = FALSE
  )
  
  format_list <- list(
    mimetype = "text/csv",
    file_name = "csv",
    header = 1,
    skip = 0,
    na.strings = "NA",
    time.row = 1,
    vars = vars_df
  )
  
  site_list <- list(id = 1, lat = 40, lon = -80, time_zone = "UTC")
  
  # 3. Test execution
  # load_data calls load_csv, renames columns, converts units (Celsius -> K), and parses time
  res <- load_data(data.path = csv_file, format = format_list, site = site_list)
  
  # Assertions
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2)
  expect_true(all(c("time", "AirT", "posix") %in% names(res))) # legacy creates posix
  
  # Check unit conversion (Celsius to Kelvin: + 273.15)
  expect_equal(res$AirT[1], 293.15)
  expect_equal(res$AirT[2], 294.15)
  
  # Check time parsing (should match AmeriFlux 30-min intervals)
  expected_time <- as.POSIXct(c("2000-01-01 00:00:00", "2000-01-01 00:30:00"), tz = "UTC")
  expect_equal(res$posix, expected_time)
  
  # Cleanup
  unlink(csv_file)
})
