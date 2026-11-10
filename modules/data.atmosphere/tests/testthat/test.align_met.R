## Tests for align.met()
##
## The bug fixed in this PR (line 431, ensemble source path):
##   rep(dat.tem, each = stamps.hr)
## where stamps.hr is a numeric *vector*.  R silently takes stamps.hr[1]
## and truncates to an integer.
##
## For hourly training data, stamps.hr = c(0.5) (the first centred hour stamp).
## Truncating 0.5 to 0 makes rep() return an empty vector, so dat.source
## ends up with zero rows -- silently discarding all source data.
##
## Fix: rep(dat.tem, each = length(stamps.hr))
## The single-time-series source path (line 304) already used length() correctly.

## Helper: create a minimal single-variable NetCDF with a given number of
## time steps placed in outdir/filename.  Time dimension is in fractional days
## since 2001-01-01, matching the step size implied by n_time.
make_align_nc <- function(n_time, outdir, filename) {
  time_dim <- ncdf4::ncdim_def(
    name  = "time",
    units = "days since 2001-01-01",
    vals  = seq(0, by = 1 / (n_time / 365), length.out = n_time)
  )
  temp_var <- ncdf4::ncvar_def(
    name    = "air_temperature",
    units   = "K",
    dim     = list(time_dim),
    missval = -9999
  )
  nc <- ncdf4::nc_create(file.path(outdir, filename), vars = list(air_temperature = temp_var))
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  ncdf4::ncatt_put(nc, 0, "description", "synthetic data for align.met test")
  ncdf4::ncvar_put(nc, temp_var, vals = seq(280, length.out = n_time, by = 0.01))
  invisible(file.path(outdir, filename))
}

test_that("align.met ensemble source path produces non-empty source data (bug: each=0 from stamps.hr truncation)", {
  train_dir  <- withr::local_tempdir()
  source_dir <- withr::local_tempdir()

  ## Hourly training (8760 steps) produces stamps.hr[1] == 0.5, intentionally
  ## chosen so that any truncation of the step count to integer yields 0.
  make_align_nc(n_time = 8760, outdir = train_dir, filename = "2001.nc")

  ## Source: daily (365 steps), placed inside an ensemble subfolder.
  ens_dir <- file.path(source_dir, "ens001")
  dir.create(ens_dir)
  make_align_nc(n_time = 365, outdir = ens_dir, filename = "2001.nc")

  result <- align.met(
    train.path  = train_dir,
    source.path = source_dir,
    n.ens       = 1,
    seed        = 20260602
  )

  ## 8760 hourly training rows; 365 source rows (one per day, repeated once each).
  expect_equal(nrow(result$dat.train$air_temperature), 8760)
  expect_equal(nrow(result$dat.source$air_temperature), 365)
})

test_that("align.met single-series source matches training row count when already aligned", {
  train_dir  <- withr::local_tempdir()
  source_dir <- withr::local_tempdir()

  ## Both training and source at the same 3-hourly resolution (2920 steps for 2001).
  make_align_nc(n_time = 2920, outdir = train_dir,  filename = "2001.nc")
  make_align_nc(n_time = 2920, outdir = source_dir, filename = "2001.nc")

  result <- align.met(
    train.path  = train_dir,
    source.path = source_dir,
    n.ens       = 1,
    seed        = 20260602
  )

  expect_equal(nrow(result$dat.source$air_temperature),
               nrow(result$dat.train$air_temperature))
})
