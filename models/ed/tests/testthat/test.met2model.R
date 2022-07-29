test_that("Met conversion runs without error", {
  outdir <- tempfile() #creates a directory
  withr::defer(unlink(outdir, recursive = TRUE))
  
  nc_path <- system.file("test-data", "CRUNCEP.2000.nc",
                         package = "PEcAn.utils")
  in.path <- dirname(nc_path)
  in.prefix <- "CRUNCEP"
  start_date <- "2000-01-01"
  end_date <- "2000-12-31"
  result <- met2model.ED2(in.path, in.prefix, outdir, start_date, end_date)
  expect_s3_class(result, "data.frame")
  expect_true(file.exists(result[["file"]][[1]]))
})