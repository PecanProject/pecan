context("Testing all workflows endpoints")

test_that("Calling /api/workflows/ with valid parameters returns Status 200", {
  res <- httr::GET(
    "http://pecan.localhost/api/workflows/?model_id=1000000022&site_id=676",
    httr::authenticate("carya", "illinois")
  )
  expect_equal(res$status, 200)
})



test_that("Calling /api/workflows/{id} with valid workflow id returns Status 200", {
  res <- httr::GET(
    "http://pecan.localhost/api/workflows/1000009172",
    httr::authenticate("carya", "illinois")
  )
  expect_equal(res$status, 200)
})

test_that("Calling /api/workflows/ with invalid parameters returns Status 404", {
  res <- httr::GET(
    "http://pecan.localhost/api/workflows/?model_id=1000000000&site_id=1000000000",
    httr::authenticate("carya", "illinois")
  )
  expect_equal(res$status, 404)
})



test_that("Calling /api/workflows/{id} with invalid workflow id returns Status 404", {
  res <- httr::GET(
    "http://pecan.localhost/api/workflows/1000000000",
    httr::authenticate("carya", "illinois")
  )
  expect_equal(res$status, 404)
})

test_that("Submitting XML workflow to /api/workflows/ returns Status 201", {
  xml_string <- paste0(xml2::read_xml("test_workflows/api.sipnet.xml"))
  res <- httr::POST(
    "http://pecan.localhost/api/workflows/",
    httr::authenticate("carya", "illinois"),
    httr::content_type("application/xml"),
    body = xml_string
  )
  expect_equal(res$status, 201)
})

test_that("Submitting JSON workflow to /api/workflows/ returns Status 201", {
  Sys.sleep(2)
  json_workflow <- jsonlite::read_json("test_workflows/api.sipnet.json")
  res <- httr::POST(
    "http://pecan.localhost/api/workflows/",
    httr::authenticate("carya", "illinois"),
    body = json_workflow,
    encode='json'
  )
  expect_equal(res$status, 201)
})

test_that("Calling /api/workflows/{id}/status with valid workflow id returns Status 200", {
  res <- httr::GET(
    paste0("http://pecan.localhost/api/workflows/", 99000000031, "/status"),
    httr::authenticate("carya", "illinois")
  )
  expect_equal(res$status, 200)
})

test_that("Calling /api/workflows/{id}/status with invalid parameters returns Status 404", {
  res <- httr::GET(
    "http://pecan.localhost/api/workflows/0/status",
    httr::authenticate("carya", "illinois")
  )
  expect_equal(res$status, 404)
})

test_that("Calling /api/workflows/{id}/file/{filename} with valid parameters returns Status 200", {
  res <- httr::GET(
    paste0("http://pecan.localhost/api/workflows/", 99000000031, "/file/", "pecan.CONFIGS.xml"),
    httr::authenticate("carya", "illinois")
  )
  expect_equal(res$status, 200)
})

test_that("Calling /api/workflows/{id}/file/{filename} with invalid parameters returns Status 404", {
  res <- httr::GET(
    "http://pecan.localhost/api/workflows/0/file/randomfile.txt",
    httr::authenticate("carya", "illinois")
  )
  expect_equal(res$status, 404)
})