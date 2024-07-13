context("Download NARR via THREDDS")

start_date <- "2012-02-20"
end_date <- "2012-03-05"
ntime <- as.numeric(difftime(end_date, start_date) + 1) * 24 / 3 + 1
lat.in <- 43.3724
lon.in <- -89.9071

test_url <- generate_narr_url(as.POSIXct("2000-01-01"), TRUE)[["url"]]
test_nc <- tryCatch(ncdf4::nc_open(test_url), error = function(e) {
  skip("Unable to reach NARR server")
})
ncdf4::nc_close(test_nc)

test_that("NARR download works as expected", {
  # Download is too slow for travis
  # Please run locally to test!
  skip_on_ci()
  skip_if_offline()
  withr::with_tempdir({
    r <- download.NARR_site(
      outfolder = getwd(),
      start_date, end_date, lat.in, lon.in,
      progress = TRUE, parallel = TRUE, ncores = 2)
    expect_equal(nrow(r), 1)
    expect_true(file.exists(r$file[1]))
    nc <- ncdf4::nc_open(r$file)
    temp <- ncdf4::ncvar_get(nc, "air_temperature")
    precip <- ncdf4::ncvar_get(nc, "precipitation_flux")
    expect_true(all(!is.na(temp)), all(temp > 0), length(temp) == ntime)
    expect_true(all(!is.na(precip)), length(precip) == ntime)
    ncdf4::nc_close(nc)
  })
})
