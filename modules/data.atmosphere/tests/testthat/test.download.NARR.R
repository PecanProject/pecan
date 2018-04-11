context("Download NARR via THREDDS")

start_date <- "2000-01-03"
end_date <- "2000-01-12"
ntime <- as.numeric(difftime(end_date, start_date) + 1) * 24 / 3 + 1
lat.in <- 43.3724
lon.in <- -89.9071
verbose <- TRUE
outfolder <- tempdir()

test_that(
  "NARR download works as expected",
  {
    r <- download.NARR_site(outfolder, start_date, end_date, lat.in, lon.in, progress = TRUE)
    expect_equal(nrow(r), 1)
    expect_true(file.exists(r$file))
    nc <- ncdf4::nc_open(r$file)
    temp <- ncdf4::ncvar_get(nc, "air_temperature")
    expect_true(all(temp > 0), length(temp) == ntime)
    ncdf4::nc_close(nc)
  }
)

unlink(outfolder, recursive = TRUE, force = TRUE)
