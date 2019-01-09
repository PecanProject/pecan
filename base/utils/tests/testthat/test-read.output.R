context("read.output")

testdir = file.path(tempfile(), "readtest")
dir.create(testdir, recursive = TRUE)
teardown(unlink(testdir, recursive = TRUE))


test_that("returns a list or dataframe as requested", {
	times_to_netcdf(0:364, "days since 2001-01-01", testdir, "2001.nc")
	
	listres <- read.output(runid = "", outdir = testdir, variables = "Y", dataframe = FALSE)
	expect_is(listres, "list")
	expect_length(listres, 1)
	expect_setequal(names(listres), "Y")
	expect_length(listres$Y, 365)

	dfres <- read.output(runid = "", outdir = testdir, variables = "Y", dataframe = TRUE)
	expect_s3_class(dfres, "data.frame")
	expect_equal(dim(dfres), c(365, 3))
	expect_setequal(names(dfres), c("posix", "year", "Y"))
	expect_s3_class(dfres$posix, "POSIXct")
	expect_equal(range(dfres$posix), as.POSIXct(c("2001-01-01", "2001-12-31"), tz="UTC"))
	expect_equal(unique(dfres$year), 2001)
})

test_that("accepts start and end years as string, number, datetime", {
	times_to_netcdf(0:364, "days since 2001-01-01", testdir, "2001.nc")
	times_to_netcdf(0:364, "days since 2002-01-01", testdir, "2002.nc")
	times_to_netcdf(0:364, "days since 2003-01-01", testdir, "2003.nc")

	res_all <- read.output(runid = "", outdir = testdir, variables = "Y", dataframe = TRUE)
	res_str <- read.output(runid = "", outdir = testdir, variables = "Y", start.year = "2001", end.year = "2003", dataframe = TRUE)
	res_num <- read.output(runid = "", outdir = testdir, variables = "Y", start.year = 2001, end.year = 2003, dataframe = TRUE)
	res_date <- read.output(runid = "", outdir = testdir, variables = "Y", start.year = as.Date("2001-01-01"), end.year = as.Date("2003-06-23"), dataframe = TRUE)
	res_posix <- read.output(runid = "", outdir = testdir, variables = "Y", start.year = as.POSIXct("2001-01-01 08:22:33"), end.year = as.POSIXct("2003-12-16 23:22:53"), dataframe = TRUE)

	expect_equivalent(res_all, res_str)
	expect_equivalent(res_all, res_num)
	expect_equivalent(res_all, res_date)
	expect_equivalent(res_all, res_posix)

	expect_length(res_all$posix, 365*3)
	expect_setequal(names(res_all), c("posix", "year", "Y"))

        out_log <- capture.output(type = "message", {
          res_start <- read.output(runid = "", outdir = testdir, variables = "Y",
                                   dataframe = TRUE, start.year = 2002)
          res_end <- read.output(runid = "", outdir = testdir, variables = "Y",
                                 dataframe = TRUE, end.year = 2002)
        })
        
        expect_length(res_start[["posix"]], 365 * 2)
        expect_length(res_end[["posix"]], 365 * 2)
})

test_that("handles arbitrary time offsets", {
	times_to_netcdf(365:730, "days since 2003-01-01", testdir, "2004.nc")
	times_to_netcdf( ((0:364)+916) * 24, "hours since 2002-06-30", testdir, "2005.nc")

	mixedres = read.output(runid = "", outdir = testdir, variables = "Y", dataframe = TRUE, start.year = 2004, end.year = 2006)

	expect_length(mixedres$posix, 731)
	
	# Hack: drop `dim` attribute so it doesn't make the next comparison fail
	dim(mixedres$posix) = NULL
	expect_equal(mixedres$posix, as.POSIXct((0:730)*86400, origin = "2004-01-01", tz = "UTC"))
})

empty_testdir <- tempfile()
dir.create(empty_testdir)
teardown(unlink(empty_testdir, recursive = TRUE))

test_that("Correct behavior when no NetCDF files present", {
  expected <- "no netCDF files of model output present"
  out_log <- capture.output(type = "message", {
    out <- read.output(runid = "", outdir = empty_testdir,
                start.year = 2001, end.year = 2002)
  })
  expect_match(out_log, "no netCDF files of model output present", all = FALSE)
  expect_match(out_log, "No files found. Returning all NA", all = FALSE)
  expect_equal(out, list(NA))
})

custom_testdir <- tempfile()
dir.create(custom_testdir)
teardown(unlink(custom_testdir, recursive = TRUE))
test_that("Correctly read all variables, from custom ncfiles", {
  options(warn = 2)
  ncfiles <- file.path(custom_testdir, c("a.nc", "b.nc", "c.nc"))
  zzz <- lapply(ncfiles, example_netcdf, varnames = c("x", "y", "z"))
  out_log <- capture.output(type = "message", {
    out <- read.output(ncfiles = ncfiles, variables = NULL)
  })
  expect_match(out_log, "No start or end year provided", all = FALSE)
  expect_match(out_log, "Invalid \\(or missing\\) origin year", all = FALSE)
  expect_match(out_log, "Reading output for all variables", all = FALSE)
  expect_match(out_log, "Result summary:", all = FALSE)
  expect_match(out_log, " +Mean +Median", all = FALSE)
  expect_named(out, c("x", "y", "z"))
  for (v in c("x", "y", "z")) expect_length(out[[!!v]], 365 * 3)
})
