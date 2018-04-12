context("Download NARR via THREDDS")

start_date <- "2012-02-20"
end_date <- "2012-03-05"
ntime <- as.numeric(difftime(end_date, start_date) + 1) * 24 / 3 + 1
lat.in <- 43.3724
lon.in <- -89.9071
outfolder <- tempdir()

r <- download.NARR_site(outfolder, start_date, end_date, lat.in, lon.in,
                        progress = TRUE, parallel = TRUE)

test_that(
  "NARR download works as expected",
  {
    expect_equal(nrow(r), 1)
    expect_true(file.exists(r$file[1]))
    nc <- ncdf4::nc_open(r$file)
    temp <- ncdf4::ncvar_get(nc, "air_temperature")
    precip <- ncdf4::ncvar_get(nc, "precipitation_flux")
    expect_true(all(!is.na(temp)), all(temp > 0), length(temp) == ntime)
    expect_true(all(!is.na(precip)), length(precip) == ntime)
    ncdf4::nc_close(nc)
  }
)

unlink(outfolder, recursive = TRUE, force = TRUE)
