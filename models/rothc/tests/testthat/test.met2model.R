test_that("met2model", {

  test_dir <- withr::local_tempdir("rothc_met_tmp")
  nc_path <- system.file(
    "test-data", "CRUNCEP.2000.nc",
    package = "PEcAn.utils"
  )
  indir <- dirname(nc_path)


  # full year
  res_1 <- met2model.RothC(
    in.path = indir,
    in.prefix = "CRUNCEP",
    outfolder = test_dir,
    start_date = "2000-01-01",
    end_date = "2000-12-31"
  )
  expect_s3_class(res_1, "data.frame")
  datfile <- res_1[["file"]][[1]]
  expect_true(file.exists(datfile))
  means_full <- read.table(datfile, header = TRUE)
  expect_identical(dim(means_full), c(12L, 5L))
  expect_identical(means_full$month, 1:12)
  expect_false(anyNA(means_full))


  # skips rewriting if overwrite not passed
  mtime_1 <- file.mtime(datfile)
  expect_output(
    met2model.RothC(
      indir, "CRUNCEP", test_dir,
      "2000-01-01", "2000-12-31"
    ),
    "already exists, skipping to next file"
  )
  expect_equal(file.mtime(datfile), mtime_1)
  met2model.RothC(
    indir, "CRUNCEP", test_dir,
    "2000-01-01", "2000-12-31",
    overwrite = TRUE
  )
  expect_gt(file.mtime(datfile), mtime_1)


  # partial year
  res_2 <- met2model.RothC(
    in.path = indir,
    in.prefix = "CRUNCEP",
    outfolder = test_dir,
    start_date = "2000-03-15",
    end_date = "2000-06-01"
  )
  datfile <- res_2[["file"]][[1]]
  expect_match(datfile, "CRUNCEP.2000-03.2000-06")
  means_summer <- read.table(datfile, header = TRUE)
  expect_identical(dim(means_summer), c(4L, 5L))
  expect_equal(means_summer, means_full[3:6, ], ignore_attr = "row.names")


  # all years unavailable
  expect_error(
    met2model.RothC(
      in.path = indir,
      in.prefix = "CRUNCEP",
      outfolder = test_dir,
      start_date = "2002-01-01",
      end_date = "2003-12-31"
    ),
    "No files found"
  )

  # some years unavailable
  expect_error(
    met2model.RothC(
      in.path = indir,
      in.prefix = "CRUNCEP",
      outfolder = test_dir,
      start_date = "2000-01-01",
      end_date = "2002-12-31"
    ),
    "does not cover"
  )

})
