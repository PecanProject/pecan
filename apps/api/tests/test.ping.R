context("Testing the /api/ping endpoint")

test_that("Calling /api/ping returns Status 200", {
  res <- httr::GET("http://pecan.localhost/api/ping")
  expect_equal(res$status, 200)
})