test_that("nc_write_varfiles", {
  outdir <- withr::local_tempdir()

  nc1 <- example_netcdf(c("a", "b"), file.path(outdir, "2015.nc"))
  nc2 <- example_netcdf(c("a", "c"), file.path(outdir, "foo.nc"))


  nc1_nm <- nc_longnames(nc1)
  expect_equal(
    nc1_nm,
    data.frame(
      name = c("a", "b"),
      longname = c("a", "b"),
      row.names = c("a", "b")
    )
  )


  nc1_varfile <- file.path(outdir, "nc1_vars.txt")
  nc_write_varfile(nc1_nm, nc1_varfile)
  expect_equal(readLines(nc1_varfile), c("a a", "b b"))


  nc_write_varfiles(outdir, write_mode = "paired")
  var_files <- file.path(outdir, c("2015.nc.var", "foo.nc.var"))
  expect_true(all(file.exists(var_files)))
  expect_equal(readLines(var_files[[1]]), c("a a", "b b"))
  expect_equal(readLines(var_files[[2]]), c("a a", "c c"))

  vars_file <- file.path(outdir, "nc_vars.txt")
  nc_write_varfiles(outdir, write_mode = "collected")
  expect_true(file.exists(vars_file))
  expect_equal(readLines(vars_file), c("a a", "b b", "c c"))
})
