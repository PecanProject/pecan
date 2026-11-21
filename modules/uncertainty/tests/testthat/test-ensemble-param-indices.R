test_that("invalid parameter indices fail before any configs are written", {
  directory <- withr::local_tempdir()
  settings <- list(run = list(inputs = list()), rundir = file.path(directory, "run"),
                   modeloutdir = file.path(directory, "out"))
  samples <- list(pft1 = data.frame(SLA = c(10, 20, 30, 40)))
  invalid <- list(c(1, NA), c(0, 1), c(-1, 1), c(1.5, 2), c(Inf, 2),
                  c(NaN, 2), c("1", "2"), 1L, c(1, 5))
  for (indices in invalid) {
    expect_error(write.ensemble.configs(
      input_design = data.frame(param = indices), ensemble.size = 2,
      defaults = list(), ensemble.samples = samples, settings = settings,
      model = "FAKE", write.to.db = FALSE),
      "input_design$param", fixed = TRUE)
    expect_false(dir.exists(settings$rundir))
  }
})

test_that("parameter indices must fit every PFT sample table", {
  samples <- list(pft1 = data.frame(SLA = 1:4), pft2 = data.frame(SLA = 1:2))
  expect_error(write.ensemble.configs(
    input_design = data.frame(param = c(1L, 3L)), ensemble.size = 2,
    defaults = list(), ensemble.samples = samples,
    settings = list(run = list(inputs = list())), model = "FAKE", write.to.db = FALSE),
    "available sample rows for PFT.*pft2")
})

test_that("a no-param design rejects a mismatched PFT table before writing configs", {
  directory <- withr::local_tempdir()
  settings <- list(
    rundir = file.path(directory, "run"), modeloutdir = file.path(directory, "out"),
    model = list(type = "FAKE"),
    run = list(inputs = list(met = list(path = "met"))),
    ensemble = list(samplingspace = list(parameters = list(method = "uniform")))
  )
  samples <- list(pft1 = data.frame(SLA = c(10, 20)),
                  pft2 = data.frame(SLA = c(30, 40, 50)))
  expect_error(write.ensemble.configs(
    input_design = data.frame(met = c(1L, 1L)), ensemble.size = 2,
    defaults = list(), ensemble.samples = samples, settings = settings,
    model = "FAKE", write.to.db = FALSE),
    "Parameter table for PFT.*pft2.*must have exactly 2 rows; got 3")
  expect_false(dir.exists(settings$rundir))
})
