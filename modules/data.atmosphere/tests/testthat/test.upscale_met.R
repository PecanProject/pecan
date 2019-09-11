context("testing upscale_met.R")

orig <- ncdf4::nc_open("data/urbana_subdaily_test.nc")
otime <- orig$dim$time
ncdf4::nc_close(orig)

tmpdir <- tempfile()
setup(dir.create(tmpdir, showWarnings = FALSE))
teardown(unlink(tmpdir, recursive = TRUE))

sc_result <- upscale_met(outfolder = tmpdir,
                         input_met = "data/urbana_subdaily_test.nc",
                         resolution = 6/24,
                         overwrite = TRUE)

scaled <- ncdf4::nc_open(sc_result$file)
stime <- scaled$dim$time
ncdf4::nc_close(scaled)

test_that("output is scaled correctly", {
  orig_dt <- mean(diff(otime$vals))
  sc_dt <- mean(diff(stime$vals))
  sc_dt_hours <- udunits2::ud.convert(
    sc_dt,
    stime$units,
    sub("^.* since", "hours since", stime$units))
  expect_equal(sc_dt_hours, 6)

  # change in file length should be proportional to change in timestep,
  # but allow truncation of incomplete timesteps at end of output 
  expect_lt(stime$len*sc_dt - otime$len*orig_dt, sc_dt)

  # date ranges should match to within one upscaled timestep.
  expect_lt(min(stime$vals) - min(otime$vals), sc_dt/orig_dt)
  expect_lt(max(otime$vals) - max(stime$vals), sc_dt/orig_dt)
})

test_that("units are preserved", {
  # only checking if preserved well enough to parse --
  # OK if not string identical
  expect_equal(udunits2::ud.convert(1, stime$units, otime$units), 1)
})
