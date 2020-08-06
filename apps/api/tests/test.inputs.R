context("Testing all inputs related endpoints")

test_that("Calling /api/inputs/ with valid parameters returns Status 200", {
  res <- httr::GET(
    "http://localhost:8000/api/inputs/?model_id=1000000022&site_id=676",
    httr::authenticate("carya", "illinois")
  )
  expect_equal(res$status, 200)
})

test_that("Calling /api/inputs/ with invalid parameters returns Status 404", {
  res <- httr::GET(
    "http://localhost:8000/api/inputs/?model_id=0&site_id=0",
    httr::authenticate("carya", "illinois")
  )
  expect_equal(res$status, 404)
})