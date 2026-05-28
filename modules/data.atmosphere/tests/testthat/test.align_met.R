## Tests for align.met()
##
## The bug fixed in this PR:
##   In the ensemble source path, when the source resolution is coarser than
##   the training resolution (align == "repeat"), the code called
##     rep(dat.tem, each = stamps.hr)
##   where stamps.hr is a numeric *vector* of hour-of-day values.  R silently
##   uses only the first element, truncated to an integer; because stamps.hr[1]
##   is always 1.5 (truncated to 1), each daily value was repeated only once
##   instead of being tiled to match every sub-daily training step.
##
##   Fix: rep(dat.tem, each = length(stamps.hr))

## Helper: create a minimal single-variable NetCDF with a given number of
## time steps, placed in outdir/filename.  The time dimension is in days.
make_align_nc <- function(n_time, outdir, filename) {
  time_dim <- ncdf4::ncdim_def(
    name  = "time",
    units = "days since 2001-01-01",
    vals  = seq(0, by = 1 / (n_time / 365), length.out = n_time)
  )
  temp_var <- ncdf4::ncvar_def(
    name   = "air_temperature",
    units  = "K",
    dim    = list(time_dim),
    missval = -9999
  )
  nc <- ncdf4::nc_create(file.path(outdir, filename), vars = list(air_temperature = temp_var))
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  ncdf4::ncatt_put(nc, 0, "description", "synthetic data for align.met test")
  ncdf4::ncvar_put(nc, temp_var, vals = seq(280, length.out = n_time, by = 0.01))
  invisible(file.path(outdir, filename))
}

test_that("align.met matches row counts when ensemble source is coarser than training", {
  skip_if_not_installed("ncdf4")
  skip_if_not_installed("withr")
  skip_if_not_installed("lubridate")

  train_dir  <- withr::local_tempdir()
  source_dir <- withr::local_tempdir()

  ## Training: 3-hourly, 2920 time steps for 2001 (non-leap year, 365 * 8)
  make_align_nc(n_time = 2920, outdir = train_dir, filename = "2001.nc")

  ## Source: daily, 365 time steps for 2001, placed inside an ensemble subfolder
  ens_dir <- file.path(source_dir, "ens001")
  dir.create(ens_dir)
  make_align_nc(n_time = 365, outdir = ens_dir, filename = "2001.nc")

  result <- align.met(
    train.path  = train_dir,
    source.path = source_dir,
    n.ens       = 1,
    seed        = 42
  )

  n_train  <- nrow(result$dat.train$air_temperature)
  n_source <- nrow(result$dat.source$air_temperature)

  ## After the fix, each daily source value is tiled 8 times to match the
  ## 3-hourly training grid, so both outputs should have 2920 rows.
  expect_equal(n_train, 2920,
    label = "training data row count")
  expect_equal(n_source, n_train,
    label = "source row count equals training row count after upsampling")
})

test_that("align.met works correctly when single-series source matches training resolution", {
  skip_if_not_installed("ncdf4")
  skip_if_not_installed("withr")
  skip_if_not_installed("lubridate")

  train_dir  <- withr::local_tempdir()
  source_dir <- withr::local_tempdir()

  ## Both at the same 3-hourly resolution, 2920 steps for 2001
  make_align_nc(n_time = 2920, outdir = train_dir,  filename = "2001.nc")
  make_align_nc(n_time = 2920, outdir = source_dir, filename = "2001.nc")

  result <- align.met(
    train.path  = train_dir,
    source.path = source_dir,
    n.ens       = 1,
    seed        = 42
  )

  expect_equal(nrow(result$dat.source$air_temperature),
               nrow(result$dat.train$air_temperature),
               label = "source and training row counts match when already aligned")
})
