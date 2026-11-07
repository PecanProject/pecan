test_that("build_SM_CDS_request uses the current CDS soil-moisture values", {
  request <- build_SM_CDS_request(as.Date("2026-11-02"))

  expect_equal(request$variable, "surface_soil_moisture_saturation")
  expect_equal(request$time_aggregation, "daily")
  expect_equal(request$year, "2026")
  expect_equal(request$month, "11")
  expect_equal(request$day, "02")
})

test_that("retrieve_SM_CDS submits the provided request to CDS", {
  received <- NULL
  client <- list(
    retrieve = function(dataset, request, target) {
      received <<- list(dataset = dataset, request = request, target = target)
      TRUE
    }
  )
  request <- build_SM_CDS_request(as.Date("2026-11-02"))

  retrieve_SM_CDS(client, request, "soil-moisture.zip", retry_delay = 0)

  expect_equal(received$dataset, "satellite-soil-moisture")
  expect_equal(received$request$variable, "surface_soil_moisture_saturation")
  expect_equal(received$request$time_aggregation, "daily")
  expect_equal(received$target, "soil-moisture.zip")
})

test_that("retrieve_SM_CDS retries transient errors only up to its limit", {
  attempts <- 0L
  client <- list(
    retrieve = function(...) {
      attempts <<- attempts + 1L
      stop("CDS rejected this request")
    }
  )

  local_mocked_bindings(
    logger.warn = function(...) NULL,
    logger.severe = function(...) stop(paste(...)),
    .package = "PEcAn.logger"
  )

  expect_error(
    retrieve_SM_CDS(client, list(), "soil-moisture.zip",
                    max_attempts = 3, retry_delay = 0),
    "failed after 3 attempts"
  )
  expect_equal(attempts, 3L)
})

test_that("retrieve_SM_CDS succeeds after a transient failure", {
  attempts <- 0L
  client <- list(
    retrieve = function(...) {
      attempts <<- attempts + 1L
      if (attempts == 1L) stop("temporary CDS error")
      TRUE
    }
  )

  local_mocked_bindings(
    logger.warn = function(...) NULL,
    .package = "PEcAn.logger"
  )

  expect_silent(
    retrieve_SM_CDS(client, list(), "soil-moisture.zip", retry_delay = 0)
  )
  expect_equal(attempts, 2L)
})
