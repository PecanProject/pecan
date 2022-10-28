context("Download ICOS data products")

outfolder <- tempdir()
setup(dir.create(outfolder, showWarnings = FALSE, recursive = TRUE))
teardown(unlink(outfolder, recursive = TRUE))

test_that("ICOS Drought 2018 download works", {
  skip_on_ci()
  skip_if_offline()

  start_date <- "2016-01-01"
  end_date <- "2017-01-01"
  sitename <- "FI-Sii"
  res <- httr::GET("https://meta.icos-cp.eu/objects/a8OW2wWfAYqZrj31S8viVLUS")
  expect_equal(200, res$status_code)
  dat <- download.ICOS(sitename, outfolder, start_date, end_date, "Drought2018", overwrite = TRUE)
  expect_true(file.exists(dat$file))
})

test_that("ICOS ETC download works", {
  skip_on_ci()
  skip_if_offline()

  start_date <- "2019-01-01"
  end_date <- "2020-01-01"
  sitename <- "FI-Sii"
  res <- httr::GET("https://meta.icos-cp.eu/objects/NEt3tFUV47QdjvJ-rgKgaiTE")
  expect_equal(200, res$status_code)
  dat <- download.ICOS(sitename, outfolder, start_date, end_date, "ETC", overwrite = TRUE)
  expect_true(file.exists(dat$file))
})
