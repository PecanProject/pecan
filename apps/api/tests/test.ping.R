context("Testing the /api/ping endpoint")

test_that("Calling /api/ping returns Status 200", {
  res <- httr::GET("http://localhost:8000/api/ping")
  expect_equal(res$status, 200)
})