test_that("annual means pair manifest records and align interval bounds", {
  root <- tempfile()
  dir.create(root)
  dir <- file.path(root, "modelout")
  runs <- expand.grid(site_id = c("north", "south"), ens_num = 1:2)
  runs$run_id <- paste0("run", 1:4)
  runs$type <- "Ensemble"
  manifest <- file.path(root, "runs_manifest.csv")
  write_manifest <- function(x) utils::write.csv(x, manifest, row.names = FALSE)
  write_manifest(runs[c(3, 2, 1, 4), ])
  write_nc <- function(i, clock = c(1.5, 4.5)) {
    run <- file.path(dir, runs$run_id[i])
    dir.create(run, recursive = TRUE, showWarnings = FALSE)
    time <- ncdf4::ncdim_def("time", "hours since 2020-01-01 00:00:00", clock)
    nv <- ncdf4::ncdim_def("nv", "", 1:2, create_dimvar = FALSE)
    vars <- list(ncdf4::ncvar_def("NEE", "kg C m-2 s-1", list(time), -999),
                 ncdf4::ncvar_def("Qle", "W m-2", list(time), -999),
                 ncdf4::ncvar_def("time_bnds", time$units, list(nv, time)))
    nc <- ncdf4::nc_create(file.path(run, "2020.nc"), vars)
    on.exit(ncdf4::nc_close(nc))
    ncdf4::ncatt_put(nc, "time", "bounds", "time_bnds")
    ncdf4::ncvar_put(nc, "time_bnds", rbind(clock - 1.5, clock + 1.5))
    ncdf4::ncvar_put(nc, "NEE", c(i, if (i == 1) 3 else NA))
    ncdf4::ncvar_put(nc, "Qle", c(if (i == 2) NA else i, i))
  }
  for (i in 1:4) write_nc(i)
  settings <- list(outdir = root, modeloutdir = dir)
  map <- data.frame(site_id = c("south", "north"), index = c(20L, 7L))
  run <- function(name, mapping = map) {
    write_site_flux_means(settings, 2020, mapping, file.path(root, name))
  }
  a <- readRDS(run("complete"))
  expect_identical(a$site_ids, map$index)
  expect_equal(as.numeric(a$time), as.numeric(as.POSIXct("2020-01-01", tz = "UTC")) + c(0, 10800))
  expect_equal(a$nee, matrix(c(3, 2, NA, 3), 2))
  expect_equal(a$le, matrix(c(4, 2, 3, 2), 2))
  expect_error(run("complete"), "already exists")
  expect_error(run("missing", map[1, ]), "north")
  write_manifest(rbind(runs, runs[1, ]))
  expect_error(run("duplicate"), "run1")
  write_manifest(runs[-2, ])
  expect_error(run("missing-member"), "member 1 for site south")
  absent <- runs
  absent$run_id[1] <- "no-output"
  write_manifest(absent)
  expect_error(run("missing-file"), "no-output")
  write_manifest(runs)
  write_nc(2, c(2.5, 5.5))
  expect_error(run("unverified-drift"), "forcing")
  write_nc(2, c(1.5, 1.5))
  expect_error(run("bad-time"), "three-hour slot")
})
