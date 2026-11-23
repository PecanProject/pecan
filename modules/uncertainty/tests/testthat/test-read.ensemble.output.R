skip_if_not_installed("ncdf4")

write_output_nc <- function(dir, run_id, values) {
  run_dir <- file.path(dir, run_id)
  dir.create(run_dir)
  time <- ncdf4::ncdim_def(
    "time", "days since 2001-01-01", seq_along(values[[1]]) - 1
  )
  vars <- lapply(names(values), function(name) {
    ncdf4::ncvar_def(name, "kg m-2 s-1", list(time), -9999)
  })
  nc <- ncdf4::nc_create(file.path(run_dir, "2001.nc"), vars)
  on.exit(ncdf4::nc_close(nc))
  for (name in names(values)) {
    ncdf4::ncvar_put(nc, name, values[[name]])
  }
}

read_members <- function(dir, run_ids, variable) {
  read.ensemble.output(
    ensemble.size = length(run_ids), pecandir = dir, outdir = dir,
    start.year = 2001, end.year = 2001, variable = variable,
    ens.run.ids = data.frame(run_id = run_ids)
  )
}

test_that("members without output stay missing regardless of their neighbors", {
  dir <- withr::local_tempdir()
  write_output_nc(dir, "a", list(GPP = c(10, 10)))
  write_output_nc(dir, "b", list(GPP = c(20, 30)))
  dir.create(file.path(dir, "empty"))
  variable <- PEcAn.utils::convert.expr("GPP")$variable.eqn

  ids <- c("empty", "a", "empty", "b", "empty")
  expect_identical(
    read_members(dir, ids, variable),
    list("1" = NA_real_, "2" = 10, "3" = NA_real_, "4" = 25, "5" = NA_real_)
  )
})

test_that("expressions can use the variables present in a partial member", {
  dir <- withr::local_tempdir()
  write_output_nc(dir, "both", list(GPP = c(10, 20), NPP = c(2, 4)))
  write_output_nc(dir, "gpp", list(GPP = c(4, 6)))
  write_output_nc(dir, "npp", list(NPP = c(3, 9)))
  ids <- c("both", "gpp", "npp", "empty")
  variable <- list(expression = "GPP + NPP", variables = c("GPP", "NPP"))

  expect_identical(
    read_members(dir, ids, variable),
    list("1" = 18, "2" = NA_real_, "3" = NA_real_, "4" = NA_real_)
  )

  variable$expression <- "sum(GPP, NPP, na.rm = TRUE)"
  expect_identical(
    read_members(dir, ids, variable),
    list("1" = 36, "2" = 10, "3" = 12, "4" = NA_real_)
  )
})

test_that("settings expressions support constants and unqualified functions", {
  dir <- withr::local_tempdir()
  write_output_nc(dir, "a", list(GPP = c(2, 4)))

  variable <- PEcAn.utils::convert.expr("scaled=2*GPP")$variable.eqn
  expect_identical(read_members(dir, "a", variable), list("1" = 6))

  variable <- PEcAn.utils::convert.expr("center=median(GPP)")$variable.eqn
  expect_identical(read_members(dir, "a", variable), list("1" = 3))
})

test_that("variables discovered from files are refreshed for each run", {
  dir <- withr::local_tempdir()
  write_output_nc(dir, "both", list(GPP = c(2, 4), NPP = c(3, 5)))
  write_output_nc(dir, "npp", list(NPP = c(7, 9)))
  variable <- list(expression = "sum(get0('GPP'), NPP, na.rm = TRUE)")

  expect_identical(
    read_members(dir, c("npp", "both", "npp"), variable),
    list("1" = 16, "2" = 14, "3" = 16)
  )
})
