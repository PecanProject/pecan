context("Download Drought 2018 product")

outfolder <- tempdir()
setup(dir.create(outfolder, showWarnings = FALSE, recursive = TRUE))
teardown(unlink(outfolder, recursive = TRUE))

test_that("Drought 2018 download works", {
  start_date <- "2016-01-01"
  end_date <- "2017-01-01"
  sitename <- "FI-Sii"
  print(outfolder)
  dat <- download.Drought2018(sitename, outfolder, start_date, end_date, overwrite = TRUE)
  expect_true(file.exists(dat$file))
})