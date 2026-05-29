context("standard_result")

if (!exists("standard_result", mode = "function")) {
  standard_result_candidates <- c(
    "base/workflow/R/standard_result.R",
    "R/standard_result.R",
    "../../R/standard_result.R",
    "/Users/hm/Desktop/pecan/base/workflow/R/standard_result.R"
  )
  standard_result_path <- standard_result_candidates[file.exists(standard_result_candidates)][1]

  if (is.na(standard_result_path)) {
    stop("Unable to locate standard_result.R for direct test execution.", call. = FALSE)
  }

  source(standard_result_path)
}

test_that("standard_result creates valid object", {
  r <- standard_result(
    tag = "met",
    paths = "/tmp/test.nc",
    input_id = 42L,
    dbfile_id = 99L,
    format = "CF Meteorology",
    source = "ERA5"
  )
  
  expect_s3_class(r, "pecan_preprocess_result")
  expect_equal(r$tag, "met")
  expect_equal(r$paths, "/tmp/test.nc")
  expect_equal(r$input_id, 42L)
  expect_equal(r$dbfile_id, 99L)
  expect_equal(r$format, "CF Meteorology")
  expect_equal(r$source, "ERA5")
  expect_equal(r$status, "success")
  expect_null(r$error_message)
})

test_that("standard_result coerces ids to integer for compatibility", {
  r <- standard_result(
    tag = "met",
    input_id = 42,
    dbfile_id = 99,
    source = "ERA5"
  )

  expect_type(r$input_id, "integer")
  expect_type(r$dbfile_id, "integer")
  expect_equal(r$input_id, 42L)
  expect_equal(r$dbfile_id, 99L)
})

test_that("standard_result validates tag inputs", {
  expect_error(standard_result(tag = "invalid"))
  expect_error(standard_result(tag = 123))
  expect_error(standard_result(tag = c("met", "soil")))
})

test_that("standard_result handles empty values", {
  r <- standard_result(tag = "soil")

  expect_equal(r$paths, character())
  expect_true(is.na(r$input_id))
  expect_true(is.na(r$dbfile_id))
  expect_equal(r$format, "")
  expect_equal(r$source, "")
  expect_equal(r$status, "success")
})

test_that("standard_result handles error status", {
  r <- standard_result(
    tag = "met",
    status = "error",
    error_message = "download failed"
  )
  
  expect_equal(r$status, "error")
  expect_equal(r$error_message, "download failed")
})

test_that("validate_standard_result returns object invisibly when valid", {
  r <- standard_result(tag = "met", status = "skipped")

  expect_identical(validate_standard_result(r), r)
})

test_that("validate_standard_result rejects invalid objects and values", {
  valid <- standard_result(tag = "met", source = "ERA5")

  expect_error(validate_standard_result(list()))

  no_class <- valid
  class(no_class) <- "list"
  expect_error(validate_standard_result(no_class), "must inherit")

  bad_paths <- valid
  bad_paths$paths <- 1
  expect_error(validate_standard_result(bad_paths), "paths")

  bad_input_id <- valid
  bad_input_id$input_id <- c(1L, 2L)
  expect_error(validate_standard_result(bad_input_id), "input_id")

  bad_dbfile_id <- valid
  bad_dbfile_id$dbfile_id <- "99"
  expect_error(validate_standard_result(bad_dbfile_id), "dbfile_id")

  bad_format <- valid
  bad_format$format <- character()
  expect_error(validate_standard_result(bad_format), "format")

  bad_source <- valid
  bad_source$source <- c("ERA5", "NARR")
  expect_error(validate_standard_result(bad_source), "source")

  bad_status <- valid
  bad_status$status <- "done"
  expect_error(validate_standard_result(bad_status), "status")

  missing_error_message <- valid
  missing_error_message$status <- "error"
  expect_error(validate_standard_result(missing_error_message), "error_message")

  unexpected_error_message <- valid
  unexpected_error_message$error_message <- "boom"
  expect_error(validate_standard_result(unexpected_error_message), "error_message")

  bad_error_message_type <- valid
  bad_error_message_type$status <- "error"
  bad_error_message_type$error_message <- 1
  expect_error(validate_standard_result(bad_error_message_type), "error_message")
})

test_that("print.pecan_preprocess_result outputs expected format for populated result", {
  r <- standard_result(tag = "met", source = "ERA5", paths = "/tmp/test.nc")

  output <- capture.output(print(r))

  expect_match(output[1], "PEcAn preprocessing result \\[met\\]")
  expect_match(output[2], "status: success")
  expect_match(output[3], "source: ERA5")
  expect_match(output[5], "paths: /tmp/test.nc")
})

test_that("print.pecan_preprocess_result handles empty paths and errors", {
  skipped <- standard_result(tag = "soil", status = "skipped")
  skipped_output <- capture.output(print(skipped))
  expect_match(skipped_output[5], "paths: <none>")

  errored <- standard_result(
    tag = "met",
    status = "error",
    error_message = "download failed"
  )
  error_output <- capture.output(print(errored))
  expect_true(any(grepl("error: download failed", error_output)))
})

test_that("as.data.frame.pecan_preprocess_result expands one row per path", {
  r <- standard_result(
    tag = "met",
    paths = c("/tmp/a.nc", "/tmp/b.nc"),
    input_id = 1L,
    source = "ERA5"
  )
  
  df <- as.data.frame(r)
  expect_equal(nrow(df), 2)
  expect_equal(df$tag, c("met", "met"))
  expect_equal(df$path, c("/tmp/a.nc", "/tmp/b.nc"))
  expect_equal(df$input_id, c(1L, 1L))
  expect_equal(df$error_message, c(NA_character_, NA_character_))
})

test_that("as.data.frame.pecan_preprocess_result handles empty paths", {
  r <- standard_result(tag = "soil")

  df <- as.data.frame(r)
  expect_equal(nrow(df), 1)
  expect_equal(df$tag, "soil")
  expect_true(is.na(df$path))
  expect_true(is.na(df$error_message))
})

test_that("standard_result serialization preserves the contract", {
  r <- standard_result(
    tag = "met",
    paths = c("/tmp/a.nc", "/tmp/b.nc"),
    input_id = 10L,
    dbfile_id = 20L,
    format = "CF Meteorology",
    source = "ERA5",
    status = "success"
  )

  restored <- unserialize(serialize(r, NULL))

  expect_s3_class(restored, "pecan_preprocess_result")
  expect_identical(restored, r)
  expect_identical(validate_standard_result(restored), restored)
})

test_that("backward compatibility supports legacy-shaped classed lists", {
  legacy <- list(
    tag = "met",
    paths = "/tmp/test.nc",
    input_id = 1L,
    dbfile_id = NA_integer_,
    format = "CF Meteorology",
    source = "ERA5",
    status = "success",
    error_message = NULL
  )
  class(legacy) <- c("pecan_preprocess_result", "list")

  expect_identical(validate_standard_result(legacy), legacy)
  expect_equal(as.data.frame(legacy)$path, "/tmp/test.nc")
})

test_that("invalid constructor values fail fast", {
  expect_error(standard_result(tag = "met", status = "error"), "error_message")
  expect_error(standard_result(tag = "met", status = "success", error_message = "boom"), "error_message")
  expect_error(standard_result(tag = "met", status = "unknown"), "status")
})