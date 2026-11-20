test_that("flux training validates configuration and resume arguments", {
  expect_error(train_flux_models(NULL, "out"), "one training YAML")
  expect_error(train_flux_models("train.yaml", "out", resume = NA), "TRUE or FALSE")
  expect_error(train_flux_models("train.yaml", "out", python = ""), "unavailable")
})

test_that("flux training passes paths and resume to the installed launcher", {
  skip_on_os("windows")
  root <- tempfile("flux training ")
  dir.create(root)
  config <- file.path(root, "station inputs.yaml")
  writeLines("{}", config)
  program <- file.path(root, "test python")
  writeLines(c("#!/bin/sh", "set -eu",
               "test \"$1\" = -B && test -f \"$2\" && test -f \"$3\"",
               "test \"$5\" = --resume",
               "mkdir -p \"$4/model_bundle\""), program)
  Sys.chmod(program, "0755")
  out <- file.path(root, "training output")
  expect_identical(train_flux_models(config, out, python = program, resume = TRUE),
                   normalizePath(file.path(out, "model_bundle")))
})
