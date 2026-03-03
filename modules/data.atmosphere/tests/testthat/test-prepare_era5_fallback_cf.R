test_that("prepare_era5_fallback_cf creates fallback CF file when fill_vars present", {
  skip_if_not_installed("ncdf4")

  tmp <- tempfile()
  dir.create(tmp)

  dirs <- list(
    era5_downloads = file.path(tmp, "era5_downloads"),
    era5_cf        = file.path(tmp, "era5_cf")
  )

  dir.create(dirs$era5_downloads, recursive = TRUE, showWarnings = FALSE)
  dir.create(dirs$era5_cf, recursive = TRUE, showWarnings = FALSE)

  # ---- Mock download.ERA5_cds (do nothing)
  old_download <- get(
    "download.ERA5_cds",
    asNamespace("PEcAn.data.atmosphere")
  )

  assignInNamespace(
    "download.ERA5_cds",
    function(...) invisible(NULL),
    ns = "PEcAn.data.atmosphere"
  )

  on.exit(
    assignInNamespace(
      "download.ERA5_cds",
      old_download,
      ns = "PEcAn.data.atmosphere"
    ),
    add = TRUE
  )

  # ---- Mock extract.nc.ERA5
  # Must create an actual .nc file in era5_cf and return that directory
  old_extract <- get(
    "extract.nc.ERA5",
    asNamespace("PEcAn.data.atmosphere")
  )

  assignInNamespace(
    "extract.nc.ERA5",
    function(...) {

      outfolder <- list(...)$outfolder

      dir.create(outfolder, recursive = TRUE, showWarnings = FALSE)

      file <- file.path(outfolder, "TEST_SITE_ERA5.nc")

      dim_time <- ncdf4::ncdim_def(
        "time", "days since 2000-01-01", 0:1
      )

      var <- ncdf4::ncvar_def(
        "air_temperature", "K",
        dim_time,
        missval = NA_real_
      )

      nc <- ncdf4::nc_create(file, vars = var)
      ncdf4::ncvar_put(nc, "air_temperature", c(279, 281))
      ncdf4::nc_close(nc)

      return(outfolder)
    },
    ns = "PEcAn.data.atmosphere"
  )

  on.exit(
    assignInNamespace(
      "extract.nc.ERA5",
      old_extract,
      ns = "PEcAn.data.atmosphere"
    ),
    add = TRUE
  )

  # ---- Run function
  result <- prepare_era5_fallback_cf(
    fill_vars  = "air_temperature",
    start_date = "2000-01-01",
    end_date   = "2000-12-31",
    site_id    = "TEST_SITE",
    site_lat   = 40,
    site_lon   = -88,
    dirs       = dirs,
    era5_user  = "dummy",
    era5_key   = "dummy",
    overwrite  = TRUE,
    verbose    = FALSE
  )

  expect_true(!is.null(result))
  expect_true(file.exists(result))
})


test_that("prepare_era5_fallback_cf returns NULL when no fill_vars", {
  result <- prepare_era5_fallback_cf(
    fill_vars  = character(0),
    start_date = "2000-01-01",
    end_date   = "2000-12-31",
    site_id    = "TEST_SITE",
    site_lat   = 40,
    site_lon   = -88,
    dirs       = list(),
    era5_user  = "dummy",
    era5_key   = "dummy"
  )

  expect_null(result)
})
