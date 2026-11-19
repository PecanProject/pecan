test_that("flux predictions validate the request before starting Python", {
  yaml <- tempfile(fileext = ".yaml")
  writeLines("{}", yaml)
  expect_error(predict_flux_maps(NULL), "one prediction YAML")
  expect_error(predict_flux_maps(yaml, resume = NA), "resume must be")
  expect_error(predict_flux_maps(yaml, python = ""), "executable unavailable")
})

test_that("flux predictions invoke the installed launcher and return a typed inventory", {
  skip_on_os("windows")
  dir <- tempfile("flux maps ")
  dir.create(dir)
  yaml <- file.path(dir, "request file.yaml")
  writeLines("{}", yaml)
  # A stand-in executable exercises the process boundary without the ML environment.
  python <- file.path(dir, "test python")
  writeLines(c(
    "#!/bin/sh",
    "set -eu",
    "test \"$1\" = -B && test -f \"$2\" && test \"$3\" = run || exit 2",
    "test \"$4\" = --config && test -f \"$5\" && test \"$6\" = --inventory || exit 2",
    "test \"$8\" = --resume || exit 2",
    "echo 'period_id,variable,start_utc,stop_utc_exclusive,path' > \"$7\"",
    "echo '139,NEE,2018-07-13T00:00:00+00:00,2018-07-29T00:00:00+00:00,/maps/nee.tif' >> \"$7\""
  ), python)
  Sys.chmod(python, mode = "0755")
  maps <- predict_flux_maps(yaml, python = python, resume = TRUE)
  expect_named(maps, c("period_id", "variable", "start_utc", "stop_utc_exclusive", "path"))
  expect_identical(maps$period_id, 139L)
  expect_identical(maps$variable, "NEE")
  expect_identical(maps$path, "/maps/nee.tif")
  expect_identical(maps$start_utc, as.POSIXct("2018-07-13", tz = "UTC"))
  expect_identical(maps$stop_utc_exclusive, as.POSIXct("2018-07-29", tz = "UTC"))
  writeLines(c("#!/bin/sh", "echo 'Missing flux inputs' >&2", "exit 3"), python)
  expect_error(predict_flux_maps(yaml, python), "exit 3.*Missing flux inputs")
})
