context("Testing the /api/workflows/ endpoint")

test_that("Calling /api/workflows/ returns Status 200", {
  res <- httr::GET(
    "http://localhost:8000/api/workflows/?model_id=1000000022&site_id=676",
    httr::authenticate("carya", "illinois")
  )
  expect_equal(res$status, 200)
})


context("Testing the /api/workflows/{id} endpoint")

test_that("Calling /api/workflows/{id} returns Status 200", {
  res <- httr::GET(
    "http://localhost:8000/api/workflows/1000009172",
    httr::authenticate("carya", "illinois")
  )
  expect_equal(res$status, 200)
})