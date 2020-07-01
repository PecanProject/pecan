context("Testing all workflows endpoints")

test_that("Calling /api/workflows/ with valid parameters returns Status 200", {
  res <- httr::GET(
    "http://localhost:8000/api/workflows/?model_id=1000000022&site_id=676",
    httr::authenticate("carya", "illinois")
  )
  expect_equal(res$status, 200)
})



test_that("Calling /api/workflows/{id} with valid workflow id returns Status 200", {
  res <- httr::GET(
    "http://localhost:8000/api/workflows/1000009172",
    httr::authenticate("carya", "illinois")
  )
  expect_equal(res$status, 200)
})

test_that("Calling /api/workflows/ with invalid parameters returns Status 404", {
  res <- httr::GET(
    "http://localhost:8000/api/workflows/?model_id=1000000000&site_id=1000000000",
    httr::authenticate("carya", "illinois")
  )
  expect_equal(res$status, 404)
})



test_that("Calling /api/workflows/{id} with invalid workflow id returns Status 404", {
  res <- httr::GET(
    "http://localhost:8000/api/workflows/1000000000",
    httr::authenticate("carya", "illinois")
  )
  expect_equal(res$status, 404)
})