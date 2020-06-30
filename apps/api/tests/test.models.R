context("Testing the /api/models/ endpoint")

test_that("Calling /api/models/ returns Status 200", {
  res <- httr::GET(
    "http://localhost:8000/api/models/?model_name=SIPNET&revision=ssr",
    httr::authenticate("carya", "illinois")
  )
  expect_equal(res$status, 200)
})