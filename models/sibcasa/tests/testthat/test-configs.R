
test_that("write.configs produces a job.sh", {

  run_id <- "12345"

  path <- withr::local_file(file.path(tempdir(), "sibcasa_test"))
  dir.create(file.path(path, "run", run_id), recursive = TRUE)

  settings <- list(
    run = list(
      site = list(lat = 45, lon = 90, met = "foo"),
      start.date = as.Date("2010-01-01"),
      end.date = as.Date("2015-01-01"),
      inputs = list(met = list(path = file.path(path, "met")))),
    host = list(
      outdir = file.path(path, "out"),
      rundir = file.path(path, "run")),
    model = list(binary = "bar"),
    rundir = file.path(path, "run"))

  expect_silent(write.config.SIBCASA(settings = settings, run.id = run_id))
  expect_true(file.exists(file.path(path, "run", run_id, "job.sh")))
  expect_true(file.exists(file.path(path, "run", run_id, "namel_sibdrv")))

  jobsh <- readLines(file.path(path, "run", run_id, "job.sh"))
  expect_match(
    jobsh,
    paste0('ln -s "', settings$model$binary, '" SiB'),
    fixed = TRUE,
    all = FALSE
  )
  expect_match(
    jobsh,
    paste0(
      "model2netcdf.SIBCASA('",
      file.path(settings$host$outdir, run_id), "', ",
      settings$run$site$lat, ", ",
      settings$run$site$lon, ", '",
      settings$run$start.date, "', '",
      settings$run$end.date, "')"
    ),
    fixed = TRUE,
    all = FALSE
  )
})
