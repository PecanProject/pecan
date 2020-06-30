context("Testing the /api/status endpoint")

test_that("Calling /api/status returns Status 200", {
  res <- httr::GET("http://localhost:8000/api/status")
  expect_equal(res$status, 200)
})