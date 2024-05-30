context("Testing all models endpoints")

test_that("Calling /api/models/ returns Status 200", {
  res <- httr::GET(
    "http://pecan.localhost/api/models/?model_name=SIPNET&revision=ssr",
    httr::authenticate("carya", "illinois")
  )
  expect_equal(res$status, 200)
})

test_that("Calling /api/models/ with invalid parameters returns Status 404", {
  res <- httr::GET(
    "http://pecan.localhost/api/models/?model_name=random&revision=random",
    httr::authenticate("carya", "illinois")
  )
  expect_equal(res$status, 404)
})

test_that("Calling /api/models/{model_id} returns Status 200", {
  res <- httr::GET(
    "http://pecan.localhost/api/models/1000000014",
    httr::authenticate("carya", "illinois")
  )
  expect_equal(res$status, 200)
})

test_that("Calling /api/models/{model_id} with invalid parameters returns Status 404", {
  res <- httr::GET(
    "http://pecan.localhost/api/models/1",
    httr::authenticate("carya", "illinois")
  )
  expect_equal(res$status, 404)
})