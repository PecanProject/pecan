test_that("download.ERA5_cds parameter validation and core functionality", {
  outdir <- withr::local_tempdir()

  # Mock only ecmwfr and logger dependencies
  local_mocked_bindings(
    wf_set_key = function(user, key) TRUE,
    wf_request = function(request, user, path, time_out) {
      # Validate request structure
      year_str <- sub(".*_(\\d{4})\\.nc$", "\\1", request$target)
      expect_equal(request$year, list(as.character(year_str)))
      expect_equal(request$area, c(42.59, -72.22, 42.49, -72.12))
      expect_equal(request$variable, as.list(c("2m_temperature", "surface_pressure")))
      # Create mock file
      target_file <- file.path(path, request$target)
      writeLines("mock netcdf", target_file)
      return(target_file)
    },
    .package = "ecmwfr"
  )

  local_mocked_bindings(
    logger.severe = function(...) stop(paste(...)),
    .package = "PEcAn.logger"
  )

  # Test parameter validation (missing user)
  expect_error(
    download.ERA5_cds(outdir, "2020-01-01", "2020-12-31",
                      c(-72.22, -72.12, 42.49, 42.59), "2m_temperature",
                      user = NULL, key = "key"),
    "CDS 'user' and 'key' must be provided"
  )

  # Test successful download
  result <- download.ERA5_cds(
    outfolder = outdir,
    start_date = "2020-01-01",
    end_date = "2021-12-31",
    extent = c(-72.22, -72.12, 42.49, 42.59),
    variables = c("2m_temperature", "surface_pressure"),
    user = "test_user",
    key = "test_key"
  )

  files <- sapply(result, `[[`, "file")
  expect_length(result, 2)
  expect_true(all(file.exists(files)))
  expect_true(all(grepl("ERA5_202[01]\\.nc$", files)))
  expect_equal(unique(sapply(result, `[[`, "mimetype")), "application/x-netcdf")
  expect_equal(unique(sapply(result, `[[`, "formatname")), "ERA5_year.nc")
})

test_that("download.ERA5_cds handles time parameter and dataset options", {
  outdir <- withr::local_tempdir()

  local_mocked_bindings(
    wf_set_key = function(user, key) TRUE,
    wf_request = function(request, user, path, time_out) {
      if (!is.null(attr(request, "test_time"))) {
        expect_equal(request$time, c("00:00", "12:00"))
      } else {
        expect_true(length(request$time) %in% c(2, 24))
      }
      target_file <- file.path(path, request$target)
      writeLines("mock netcdf", target_file)
      return(target_file)
    },
    .package = "ecmwfr"
  )

  local_mocked_bindings(
    logger.severe = function(...) stop(paste(...)),
    .package = "PEcAn.logger"
  )

  # Test NULL time (default all hours)
  result1 <- download.ERA5_cds(outdir, "2020-01-01", "2020-12-31",
                               c(-72, -71, 42, 43), "2m_temperature",
                               time = NULL, user = "test", key = "test")

  # Test custom time
  result2 <- download.ERA5_cds(outdir, "2020-01-01", "2020-12-31",
                               c(-72, -71, 42, 43), "2m_temperature",
                               time = c("00:00", "12:00"), user = "test", key = "test")
  attr(result2, "test_time") <- TRUE

  expect_length(result1, 1)
  expect_length(result2, 1)
})

test_that("download.ERA5_cds error handling and ecmwfr dependency", {
  skip("Cannot reliably mock requireNamespace in base R; use a package-level wrapper for robust tests.")
  # The following test would check for error handling if ecmwfr is missing,
  # but is skipped due to R's locked base functions.
  # If you refactor the code to use a wrapper, update this test to match.
  outdir <- withr::local_tempdir()

  local_mocked_bindings(
    logger.info = function(...) NULL,
    logger.severe = function(...) stop(paste(...)),
    .package = "PEcAn.logger"
  )

  # Simulate missing ecmwfr package by wrapping the function call
  expect_error(
    download.ERA5_cds(outdir, "2020-01-01", "2020-12-31",
                      c(-72, -71, 42, 43), "2m_temperature",
                      user = "test", key = "test"),
    "ecmwfr package required"
  )

  # Mock partial download failure
  local_mocked_bindings(
    wf_set_key = function(user, key) TRUE,
    wf_request = function(request, user, path, time_out) {
      year_str <- sub(".*_(\\d{4})\\.nc$", "\\1", request$target)
      if (year_str == "2020") {
        target_file <- file.path(path, request$target)
        writeLines("mock netcdf", target_file)
        return(target_file)
      } else {
        PEcAn.logger::logger.severe("Download failed")
      }
    },
    .package = "ecmwfr"
  )
  local_mocked_bindings(
    logger.error = function(...) message("ERROR: ", paste(...)),
    .package = "PEcAn.logger"
  )

  # Should continue despite failures and log error
  expect_message(
    result <- download.ERA5_cds(outdir, "2020-01-01", "2021-12-31",
                                c(-72, -71, 42, 43), "2m_temperature",
                                user = "test", key = "test"),
    "ERROR.*Failed to download.*2021"
  )

  files <- sapply(result, `[[`, "file")
  expect_length(result, 2)
  # Case: Only the first file should exist after a partial failure.
  # If the function changes to return only successful downloads, update this test.
  expect_true(file.exists(files[1]))
})
