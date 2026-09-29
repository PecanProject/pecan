
context("Preprocess Result Constructor")

test_that("preprocess_result creates valid object", {
  tmpfile <- tempfile(fileext = ".nc")
  file.create(tmpfile)
  on.exit(unlink(tmpfile))

  result <- preprocess_result(
    file = tmpfile,
    format = "CFmet",
    mimetype = "application/x-netcdf",
    dbfile.id = 12345,
    start_date = as.POSIXct("2004-01-01"),
    end_date = as.POSIXct("2004-12-31"),
    source = "Ameriflux"
  )

  expect_type(result, "list")
  expect_equal(result$file, tmpfile)
  expect_equal(result$format, "CFmet")
  expect_equal(result$dbfile.id, 12345)
  expect_s3_class(result, "preprocess_result")
  expect_s3_class(result, "list")
})

test_that("preprocess_result validates inputs with logger.severe", {
  # logger.severe stops execution, so we expect error
  expect_error(
    preprocess_result(file = 123, format = "CFmet", mimetype = "application/x-netcdf"),
    "file must be a single character path"
  )

  expect_error(
    preprocess_result(file = "foo.nc", format = NULL, mimetype = "application/x-netcdf"),
    "format must be a single character string"
  )
})

test_that("preprocess_result accepts missing files", {
  result <- preprocess_result(
    file = "/nonexistent/file.nc",
    format = "CFmet",
    mimetype = "application/x-netcdf"
  )
  
  expect_equal(result$file, "/nonexistent/file.nc")
})

test_that("insert_preprocess_result patches settings correctly", {
  settings <- list(run = list(inputs = list()))
  tmpfile <- tempfile(fileext = ".nc")
  file.create(tmpfile)
  on.exit(unlink(tmpfile))

  result <- preprocess_result(
    file = tmpfile,
    format = "CFmet",
    mimetype = "application/x-netcdf"
  )

  updated <- insert_preprocess_result(settings, result, "met")

  expect_equal(updated$run$inputs$met$path, tmpfile)
  expect_equal(updated$run$inputs$met$format, "CFmet")
})

test_that("insert_preprocess_result handles multiple results", {
  settings <- list(run = list(inputs = list()))
  tmpfile1 <- tempfile(fileext = ".nc")
  tmpfile2 <- tempfile(fileext = ".nc")
  file.create(tmpfile1)
  file.create(tmpfile2)
  on.exit({
    unlink(tmpfile1)
    unlink(tmpfile2)
  })

  results <- list(
    preprocess_result(file = tmpfile1, format = "CFmet", mimetype = "application/x-netcdf"),
    preprocess_result(file = tmpfile2, format = "CFmet", mimetype = "application/x-netcdf")
  )

  updated <- insert_preprocess_result(settings, results, "met")

  expect_equal(updated$run$inputs$met$path, c(tmpfile1, tmpfile2))
})

test_that("validate_preprocess_result catches invalid results", {
  expect_error(
    validate_preprocess_result(list(format = "CFmet")),
    "Missing required field: file"
  )

  tmpfile <- tempfile()
  file.create(tmpfile)
  on.exit(unlink(tmpfile))

  valid <- preprocess_result(file = tmpfile, format = "CFmet", mimetype = "application/x-netcdf")
  expect_true(validate_preprocess_result(valid))
})