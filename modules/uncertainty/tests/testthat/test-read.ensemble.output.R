# Regression tests for read.ensemble.output missing members.
# These use real NetCDF files so the unnamed list(NA) from read.output
# is what the ensemble reader actually receives.

write_output_nc <- function(dir, run_id, values) {
  run_dir <- file.path(dir, run_id)
  dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
  if (is.null(values)) {
    return(invisible(run_dir))
  }

  time <- ncdf4::ncdim_def(
    "time",
    "days since 2001-01-01",
    seq_along(values[[1]]) - 1
  )
  vars <- lapply(names(values), function(name) {
    ncdf4::ncvar_def(name, "kg m-2 s-1", list(time), -9999)
  })
  nc <- ncdf4::nc_create(file.path(run_dir, "2001.nc"), vars)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  for (name in names(values)) {
    ncdf4::ncvar_put(nc, name, values[[name]])
  }
  invisible(run_dir)
}

read_members <- function(dir, run_ids, expression = "GPP", variables = "GPP") {
  skip_if_not_installed("ncdf4")
  read.ensemble.output(
    ensemble.size = length(run_ids),
    pecandir = dir,
    outdir = dir,
    start.year = 2001,
    end.year = 2001,
    variable = list(expression = expression, variables = variables),
    ens.run.ids = data.frame(run_id = run_ids)
  )
}

test_that("each present member keeps its own mean", {
  dir <- withr::local_tempdir()
  write_output_nc(dir, "1", list(GPP = c(10, 10)))
  write_output_nc(dir, "2", list(GPP = c(20, 30)))

  result <- read_members(dir, c("1", "2"))

  expect_equal(result[["1"]], 10)
  expect_equal(result[["2"]], 25)
})

test_that("a missing member is NA and does not follow its neighbor", {
  dir <- withr::local_tempdir()
  write_output_nc(dir, "1", list(GPP = c(10, 10)))
  write_output_nc(dir, "2", NULL)

  PEcAn.logger::logger.setUseConsole(TRUE, FALSE)
  on.exit(PEcAn.logger::logger.setUseConsole(TRUE, TRUE), add = TRUE)
  log <- capture.output(result <- read_members(dir, c("1", "2")))

  expect_equal(result[["1"]], 10)
  expect_identical(result[["2"]], NA_real_)
  expect_match(paste(log, collapse = " "), "Ensemble run 2", fixed = TRUE)
  expect_match(paste(log, collapse = " "), "has no output for GPP", fixed = TRUE)

  unlink(file.path(dir, "1", "2001.nc"))
  write_output_nc(dir, "1", list(GPP = c(20, 20)))
  changed <- read_members(dir, c("1", "2"))
  expect_equal(changed[["1"]], 20)
  expect_identical(changed[["2"]], NA_real_)
})

test_that("a missing first member does not block the next member", {
  dir <- withr::local_tempdir()
  write_output_nc(dir, "1", NULL)
  write_output_nc(dir, "2", list(GPP = c(4, 6)))

  result <- read_members(dir, c("1", "2"))

  expect_identical(result[["1"]], NA_real_)
  expect_equal(result[["2"]], 5)
})

test_that("only the missing middle member is NA", {
  dir <- withr::local_tempdir()
  write_output_nc(dir, "1", list(GPP = c(2, 4)))
  write_output_nc(dir, "2", NULL)
  write_output_nc(dir, "3", list(GPP = c(8, 10)))

  result <- read_members(dir, c("1", "2", "3"))

  expect_equal(result[["1"]], 3)
  expect_identical(result[["2"]], NA_real_)
  expect_equal(result[["3"]], 9)
})

test_that("a file missing one variable in an expression is NA", {
  dir <- withr::local_tempdir()
  write_output_nc(dir, "1", list(GPP = c(10, 10), NPP = c(1, 3)))
  write_output_nc(dir, "2", list(GPP = c(100, 100)))

  result <- read_members(
    dir, c("1", "2"),
    expression = "GPP+NPP",
    variables = c("GPP", "NPP")
  )

  expect_equal(result[["1"]], 12)
  expect_identical(result[["2"]], NA_real_)
})

test_that("an expression is evaluated from that member only", {
  dir <- withr::local_tempdir()
  write_output_nc(dir, "1", list(GPP = c(10, 10), NPP = c(1, 3)))
  write_output_nc(dir, "2", list(GPP = c(4, 6), NPP = c(1, 1)))

  result <- read_members(
    dir, c("1", "2"),
    expression = "GPP+NPP",
    variables = c("GPP", "NPP")
  )

  expect_equal(result[["1"]], 12)
  expect_equal(result[["2"]], 6)
})

test_that("a single missing member is NA instead of an error", {
  dir <- withr::local_tempdir()
  write_output_nc(dir, "1", NULL)

  result <- read_members(dir, "1")

  expect_identical(result[["1"]], NA_real_)
})
