context("Testing all PFTs endpoints")

test_that("Calling /api/pfts/ returns Status 200", {
  res <- httr::GET(
    "http://pecan.localhost/api/pfts/?pft_name=temperate&pft_type=plant&model_type=sipnet",
    httr::authenticate("carya", "illinois")
  )
  expect_equal(res$status, 200)
})

test_that("Calling /api/pfts/ with invalid parameters returns Status 404", {
  res <- httr::GET(
    "http://pecan.localhost/api/pfts/?pft_name=random&model_type=random",
    httr::authenticate("carya", "illinois")
  )
  expect_equal(res$status, 404)
})

test_that("Calling /api/pfts/{pft_id} returns Status 200", {
  res <- httr::GET(
    "http://pecan.localhost/api/pfts/2000000045",
    httr::authenticate("carya", "illinois")
  )
  expect_equal(res$status, 200)
})

test_that("Calling /api/pfts/{pft_id} with invalid parameters returns Status 404", {
  res <- httr::GET(
    "http://pecan.localhost/api/pfts/0",
    httr::authenticate("carya", "illinois")
  )
  expect_equal(res$status, 404)
})