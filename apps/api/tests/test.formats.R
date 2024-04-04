context("Testing all formats related endpoints")

test_that("Calling /api/formats/ with valid parameters returns Status 200", {
  res <- httr::GET(
    "http://pecan.localhost/api/formats/?format_name=ameriflux&mimetype=csv&ignore_case=TRUE",
    httr::authenticate("carya", "illinois")
  )
  expect_equal(res$status, 200)
})

test_that("Calling /api/formats/ with invalid parameters returns Status 404", {
  res <- httr::GET(
    "http://pecan.localhost/api/formats/?format_name=random&mimetype=random&ignore_case=TRUE",
    httr::authenticate("carya", "illinois")
  )
  expect_equal(res$status, 404)
})

test_that("Calling /api/formats/{format_id} returns Status 200", {
  res <- httr::GET(
    "http://pecan.localhost/api/formats/19",
    httr::authenticate("carya", "illinois")
  )
  expect_equal(res$status, 200)
})

test_that("Calling /api/formats/{format_id} with invalid parameters returns Status 404", {
  res <- httr::GET(
    "http://pecan.localhost/api/formats/0",
    httr::authenticate("carya", "illinois")
  )
  expect_equal(res$status, 404)
})