context("PEPRMT met2model")

outfolder <- tempfile()
setup(dir.create(outfolder, showWarnings = FALSE))
teardown(unlink(outfolder, recursive = TRUE))

test_that("PEPRMT met conversion runs without error", {
  nc_path <- system.file("test-data", "CRUNCEP.2000.nc",
                         package = "PEcAn.utils")
  #This is the only test dataset
  #list.files(paste0(system.file(package = "PEcAn.utils"),"/test-data"))
  system.file("test-data", package = "PEcAn.utils")
  in.path <- dirname(nc_path)
  in.prefix <- "CRUNCEP"
  start_date <- "2000-01-01"
  end_date <- "2000-12-31"
  result <- met2model.PEPRMT(in.path, in.prefix, outfolder, start_date, end_date)
  expect_s3_class(result, "data.frame")
  expect_true(file.exists(result[["file"]][[1]]))
})
