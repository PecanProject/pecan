context("Testing all inputs related endpoints")

test_that("Calling /api/inputs/ with valid parameters returns Status 200", {
  res <- httr::GET(
    "http://pecan.localhost/api/inputs/?model_id=1000000022&site_id=676",
    httr::authenticate("carya", "illinois")
  )
  expect_equal(res$status, 200)
})

test_that("Calling /api/inputs/ with invalid parameters returns Status 404", {
  res <- httr::GET(
    "http://pecan.localhost/api/inputs/?model_id=0&site_id=0",
    httr::authenticate("carya", "illinois")
  )
  expect_equal(res$status, 404)
})

test_that("Calling /api/inputs/{input_id} with valid parameters returns Status 200", {
  res <- httr::GET(
    paste0("http://pecan.localhost/api/inputs/", 99000000003),
    httr::authenticate("carya", "illinois")
  )
  expect_equal(res$status, 200)
})

test_that("Calling /api/inputs/{input_id} with invalid parameters returns Status 404", {
  res <- httr::GET(
    "http://pecan.localhost/api/inputs/0",
    httr::authenticate("carya", "illinois")
  )
  expect_equal(res$status, 404)
})

test_that("Calling /api/inputs/{input_id}?filename={filename} with valid parameters returns Status 200", {
  res <- httr::GET(
    paste0("http://pecan.localhost/api/inputs/295?filename=fraction.plantation"),
    httr::authenticate("carya", "illinois")
  )
  expect_equal(res$status, 200)
})

test_that("Calling /api/inputs/{input_id}?filename={filename} with invalid parameters returns Status 404", {
  res <- httr::GET(
    "http://pecan.localhost/api/inputs/295?filename=random",
    httr::authenticate("carya", "illinois")
  )
  expect_equal(res$status, 400)
})
