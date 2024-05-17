context("Testing all sites endpoints")

test_that("Calling /api/sites/ returns Status 200", {
  res <- httr::GET(
    "http://pecan.localhost/api/sites/?sitename=washington",
    httr::authenticate("carya", "illinois")
  )
  expect_equal(res$status, 200)
})

test_that("Calling /api/sites/ with invalid parameters returns Status 404", {
  res <- httr::GET(
    "http://pecan.localhost/api/sites/?sitename=random",
    httr::authenticate("carya", "illinois")
  )
  expect_equal(res$status, 404)
})

test_that("Calling /api/sites/{site_id} returns Status 200", {
  res <- httr::GET(
    "http://pecan.localhost/api/sites/676",
    httr::authenticate("carya", "illinois")
  )
  expect_equal(res$status, 200)
})

test_that("Calling /api/sites/{site_id} with invalid parameters returns Status 404", {
  res <- httr::GET(
    "http://pecan.localhost/api/sites/0",
    httr::authenticate("carya", "illinois")
  )
  expect_equal(res$status, 404)
})