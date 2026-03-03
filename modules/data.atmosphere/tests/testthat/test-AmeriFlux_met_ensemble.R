test_that("AmeriFlux_met_ensemble integrates fallback + gapfill pipeline correctly", {
  skip_if_not_installed("ncdf4")
  skip_if_not_installed("amerifluxr")

  tmp <- tempdir()

  # Fake AmeriFlux CSV
  amf_dir <- file.path(tmp, "amf_downloads")
  dir.create(amf_dir, recursive = TRUE, showWarnings = FALSE)

  csv_file <- file.path(amf_dir, "AMF_TEST_SITE_dummy.csv")

  writeLines(
    c(
      "dummy header 1",
      "dummy header 2",
      "TIMESTAMP_START,TA_1",
      "200001010000,280",
      "200001020000,281"
    ),
    csv_file
  )


  # Mock amerifluxr::amf_site_info
  old_amf <- get("amf_site_info", asNamespace("amerifluxr"))

  assignInNamespace(
    "amf_site_info",
    function() {
      data.frame(
        SITE_ID = "TEST_SITE",
        LOCATION_LAT = 40,
        LOCATION_LONG = -88,
        stringsAsFactors = FALSE
      )
    },
    ns = "amerifluxr"
  )

  on.exit(assignInNamespace("amf_site_info", old_amf, ns = "amerifluxr"), add = TRUE)


  # Mock ERA5 fallback preparation
  old_prepare <- get(
    "prepare_era5_fallback_cf",
    asNamespace("PEcAn.data.atmosphere")
  )

  assignInNamespace(
    "prepare_era5_fallback_cf",
    function(...) {
      dirs <- list(...)$dirs
      file <- file.path(dirs$era5_cf, "fallback.nc")

      dim_time <- ncdf4::ncdim_def(
        "time", "days since 2000-01-01", 0:1
      )

      var_tair <- ncdf4::ncvar_def(
        "air_temperature", "K",
        dim_time,
        missval = NA_real_
      )

      nc <- ncdf4::nc_create(file, vars = var_tair)
      ncdf4::ncvar_put(nc, "air_temperature", c(279, 281))
      ncdf4::nc_close(nc)

      file
    },
    ns = "PEcAn.data.atmosphere"
  )

  on.exit(assignInNamespace("prepare_era5_fallback_cf", old_prepare, ns = "PEcAn.data.atmosphere"), add = TRUE)

  # Mock CF conversion
  old_met2cf <- get("met2CF.AmerifluxLBL", asNamespace("PEcAn.data.atmosphere"))

  assignInNamespace(
    "met2CF.AmerifluxLBL",
    function(...) {
      outfolder <- list(...)$outfolder
      file <- file.path(outfolder, "test_cf.nc")

      dim_time <- ncdf4::ncdim_def(
        "time", "days since 2000-01-01", 0:1
      )

      var_tair <- ncdf4::ncvar_def(
        "air_temperature", "K",
        dim_time,
        missval = NA_real_
      )

      nc <- ncdf4::nc_create(file, vars = var_tair)
      ncdf4::ncvar_put(nc, "air_temperature", c(280, NA))
      ncdf4::nc_close(nc)

      list(file = file)
    },
    ns = "PEcAn.data.atmosphere"
  )

  on.exit(assignInNamespace("met2CF.AmerifluxLBL", old_met2cf, ns = "PEcAn.data.atmosphere"), add = TRUE)


  # Mock coverage check (local function)
  old_cov <- check_met_coverage_for_fallback

  assign(
    "check_met_coverage_for_fallback",
    function(...) list(fill_vars = "air_temperature"),
    envir = parent.env(environment(AmeriFlux_met_ensemble))
  )

  on.exit(assign("check_met_coverage_for_fallback", old_cov,
    envir = parent.env(environment(AmeriFlux_met_ensemble))
  ), add = TRUE)


  # Mock ensemble generator
  old_ens <- get(
    "met_temporal_downscale.Gaussian_ensemble",
    asNamespace("PEcAn.data.atmosphere")
  )

  assignInNamespace(
    "met_temporal_downscale.Gaussian_ensemble",
    function(...) {
      outfolder <- list(...)$outfolder
      file <- file.path(outfolder, "ens_1.nc")
      file.create(file)
      list(list(file = file))
    },
    ns = "PEcAn.data.atmosphere"
  )

  on.exit(assignInNamespace(
    "met_temporal_downscale.Gaussian_ensemble",
    old_ens,
    ns = "PEcAn.data.atmosphere"
  ), add = TRUE)


  # Run pipeline
  result <- AmeriFlux_met_ensemble(
    site_id = "TEST_SITE",
    start_date = "2000-01-01",
    end_date = "2000-12-31",
    outfolder = tmp,
    ameriflux_useremail = "test@test.com",
    n_ens = 1,
    verbose = FALSE
  )


  # Assertions
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 1)
  expect_true(file.exists(result$file))
  expect_equal(result$mimetype, "application/x-netcdf")
  expect_equal(result$formatname, "CF Meteorology")
})
