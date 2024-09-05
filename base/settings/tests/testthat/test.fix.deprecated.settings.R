PEcAn.logger::logger.setQuitOnSevere(FALSE)
PEcAn.logger::logger.setLevel("OFF")
context("fix.deprecated.settings")

test_that("deprecated jobtemplate settings handled correctly", {
  settings <- .get.test.settings()
  settings$run$jobtemplate <- "somefile"
  settings$model$jobtemplate <- "otherfile"
  expect_error(fix.deprecated.settings(settings))

  settings$model$jobtemplate <- NULL
  settings <- fix.deprecated.settings(settings)
  expect_equal(settings$model$jobtemplate, "somefile")
  expect_null(settings$run$jobtemplate)

  settings <- fix.deprecated.settings(settings)
  expect_equal(settings$model$jobtemplate, "somefile")
  expect_null(settings$run$jobtemplate)
})


test_that("deprecated dbfiles settings handled correctly", {
  settings <- .get.test.settings()
  settings$run$dbfiles <- "somefile"
  settings$database$dbfiles <- "otherfile"
  expect_error(fix.deprecated.settings(settings))

  settings$database$dbfiles <- NULL
  settings <- fix.deprecated.settings(settings)
  expect_equal(settings$database$dbfiles, "somefile")
  expect_null(settings$run$dbfiles)

  settings <- fix.deprecated.settings(settings)
  expect_equal(settings$database$dbfiles, "somefile")
  expect_null(settings$run$dbfiles)
})

test_that("deprecated host settings handled correctly", {
  settings <- .get.test.settings()
  host <- list(name = "localhost")
  settings$run$host <- host
  settings$host <- host
  expect_error(fix.deprecated.settings(settings))

  settings$host <- NULL
  settings <- fix.deprecated.settings(settings)
  expect_equal(settings$host, host)
  expect_null(settings$run$dbfiles)

  settings <- fix.deprecated.settings(settings)
  expect_equal(settings$host, host)
  expect_null(settings$run$dbfiles)
})
