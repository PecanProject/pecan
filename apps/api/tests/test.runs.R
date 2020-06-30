context("Testing the /api/runs/ endpoint")

test_that("Calling /api/runs/ returns Status 200", {
  res <- httr::GET(
    "http://localhost:8000/api/runs/?workflow_id=1000009172",
    httr::authenticate("carya", "illinois")
  )
  expect_equal(res$status, 200)
})


context("Testing the /api/runs/{id} endpoint")

test_that("Calling /api/runs/{id} returns Status 200", {
  res <- httr::GET(
    "http://localhost:8000/api/runs/1002042201",
    httr::authenticate("carya", "illinois")
  )
  expect_equal(res$status, 200)
})