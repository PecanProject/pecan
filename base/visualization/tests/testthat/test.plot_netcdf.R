test_that("`data.fetch()` able to return aggregated data with the correct label", {
  nc <- ncdf4::nc_open("./data/urbana_subdaily_test.nc")
  on.exit(ncdf4::nc_close(nc))
  
  res <- data.fetch("time", nc, mean)
  expect_equal(attr(res, "lbl"), "days since 1700-01-01T00:00:00Z")

  res <- data.fetch("air_temperature", nc, mean)
  expect_equal(attr(res, "lbl"), "3-hourly Air Temperature at 2m in K")
})

test_that("`plot_netcdf()` able to correctly plot the netcdf file data and create the plot file at the desired location", {
  nc <- ncdf4::nc_open("./data/urbana_subdaily_test.nc")
  withr::with_dir(tempdir(), {
    mockery::stub(plot_netcdf, 'ncdf4::nc_open', nc)
    res <- plot_netcdf("./data/urbana_subdaily_test.nc", "time", "air_temperature", year = 2010)

    # check if file exists
    expect_true(file.exists("Rplots.pdf"))
  })
})