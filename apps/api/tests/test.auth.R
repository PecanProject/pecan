context("Testing authentication for API")

test_that("Using correct username & password returns Status 200", {
  res <- httr::GET(
    "http://pecan.localhost/api/models/",
    httr::authenticate("carya", "illinois")
  )
  expect_equal(res$status, 200)
})

test_that("Using incorrect username & password returns Status 401", {
  res <- httr::GET(
    "http://pecan.localhost/api/models/",
    httr::authenticate("carya", "wrong_password")
  )
  expect_equal(res$status, 401)
})

test_that("Not using username & password returns Status 401", {
  res <- httr::GET(
    "http://pecan.localhost/api/models/",
  )
  expect_equal(res$status, 401)
})