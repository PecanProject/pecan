context("Testing all runs endpoints")

test_that("Calling /api/runs/ with a valid workflow id returns Status 200", {
  res <- httr::GET(
    "http://pecan.localhost/api/runs/?workflow_id=1000009172",
    httr::authenticate("carya", "illinois")
  )
  expect_equal(res$status, 200)
})



test_that("Calling /api/runs/{id} with a valid run id returns Status 200", {
  res <- httr::GET(
    "http://pecan.localhost/api/runs/1002042201",
    httr::authenticate("carya", "illinois")
  )
  expect_equal(res$status, 200)
})

test_that("Calling /api/runs/ with a invalid workflow id returns Status 404", {
  res <- httr::GET(
    "http://pecan.localhost/api/runs/?workflow_id=1000000000",
    httr::authenticate("carya", "illinois")
  )
  expect_equal(res$status, 404)
})

test_that("Calling /api/runs/{id} with a invalid run id returns Status 404", {
  res <- httr::GET(
    "http://pecan.localhost/api/runs/1000000000",
    httr::authenticate("carya", "illinois")
  )
  expect_equal(res$status, 404)
})

test_that("Calling /api/runs/{run_id}/graph/{year}/{yvar}/ with valid inputs returns Status 200", {
  res <- httr::GET(
    "http://pecan.localhost/api/runs/99000000282/graph/2002/GPP",
    httr::authenticate("carya", "illinois")
  )
  expect_equal(res$status, 200)
})

test_that("Calling /api/runs/{run_id}/graph/{year}/{yvar}/ with valid inputs returns Status 200", {
  res <- httr::GET(
    "http://pecan.localhost/api/runs/1000000000/graph/100/GPP",
    httr::authenticate("carya", "illinois")
  )
  expect_equal(res$status, 404)
})

test_that("Calling /api/runs/{run_id}/input/{filename} with valid inputs returns Status 200", {
  res <- httr::GET(
    "http://pecan.localhost/api/runs/99000000282/input/sipnet.in",
    httr::authenticate("carya", "illinois")
  )
  expect_equal(res$status, 200)
})

test_that("Calling /api/runs/{run_id}/input/{filename} with valid inputs returns Status 200", {
  res <- httr::GET(
    "http://pecan.localhost/api/runs/1000000000/input/randomfile",
    httr::authenticate("carya", "illinois")
  )
  expect_equal(res$status, 404)
})

test_that("Calling /api/runs/{run_id}/output/{filename} with valid inputs returns Status 200", {
  res <- httr::GET(
    "http://pecan.localhost/api/runs/99000000282/output/2002.nc",
    httr::authenticate("carya", "illinois")
  )
  expect_equal(res$status, 200)
})

test_that("Calling /api/runs/{run_id}/output/{filename} with valid inputs returns Status 200", {
  res <- httr::GET(
    "http://pecan.localhost/api/runs/1000000000/output/randomfile",
    httr::authenticate("carya", "illinois")
  )
  expect_equal(res$status, 404)
})
