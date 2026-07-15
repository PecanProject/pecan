context("met2model")

outfolder <- tempfile()
setup(dir.create(outfolder, showWarnings = FALSE))
teardown(unlink(outfolder, recursive = TRUE))

add_gaps_to_nc <- function(src_nc, gapped_nc,
                           indices = c(1:5, 10, 15:30),
                           varname = "eastward_wind") {
  file.copy(src_nc, gapped_nc)
  nc <- ncdf4::nc_open(gapped_nc, write = TRUE)
  v <- ncdf4::ncvar_get(nc, varname)
  v[indices] <- NA
  ncdf4::ncvar_put(nc, varname, v)
  ncdf4::nc_close(nc)
}

test_that("Met conversion runs without error", {
  nc_path <- system.file(
    "test-data",
    "CRUNCEP.2000.nc",
    package = "PEcAn.utils"
  )
  in.path <- dirname(nc_path)
  in.prefix <- "CRUNCEP"
  start_date <- "2000-01-01"
  end_date <- "2000-12-31"
  result <- met2model.SIPNET(in.path, in.prefix, outfolder, start_date, end_date)
  expect_s3_class(result, "data.frame")
  expect_true(file.exists(result[["file"]][[1]]))
})

test_that("Missing data throws an error", {
  full_nc <- system.file("test-data", "CRUNCEP.2000.nc", package = "PEcAn.utils")
  withr::with_tempdir({
    add_gaps_to_nc(full_nc, "gapped.2000.nc")
    msg <- capture.output(
      result <- met2model.SIPNET(
        in.path = ".",
        in.prefix = "gapped",
        outfolder = ".",
        start_date = "2000-01-01",
        end_date = "2000-12-31"
      ),
      type = "message"
    )
    expect_match(msg, "22 (of 1464 total) rows", all = FALSE, fixed = TRUE)
    expect_null(result)
    expect_length(list.files(".", "*.clim"), 0)
  })
})


test_that("clim format switch", {
  full_nc <- system.file("test-data", "CRUNCEP.2000.nc", package = "PEcAn.utils")

  outdir1 <- withr::local_tempdir()
  outdir2 <- withr::local_tempdir()

  res1 <- met2model.SIPNET(
    in.path = dirname(full_nc),
    in.prefix = "CRUNCEP",
    outfolder = outdir1,
    start_date = "2000-01-01",
    end_date = "2000-12-31",
    clim_format_version = "v1"
  )
  res2 <- met2model.SIPNET(
    in.path = dirname(full_nc),
    in.prefix = "CRUNCEP",
    outfolder = outdir2,
    start_date = "2000-01-01",
    end_date = "2000-12-31",
    clim_format_version = "v2"
  )

  expect_true(file.exists(res1$file[[1]]))
  clim1 <- read.table(res1$file[[1]], header = FALSE, sep = "")
  expect_equal(ncol(clim1), 14)
  expect_true(all(clim1[, 1] == 0))
  expect_true(all(clim1[, 14] == 0.6))

  expect_true(file.exists(res2$file[[1]]))
  clim2 <- read.table(res2$file[[1]], header = FALSE, sep = "")
  expect_equal(ncol(clim2), 12)
  expect_true(all(clim1[, 2:13] == clim2))
})