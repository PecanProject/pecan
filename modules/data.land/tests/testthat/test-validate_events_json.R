context("validate_events_json")

testthat::test_that("urea-only fertilizer events validate and negative urea is rejected", {
    testthat::skip_if_not_installed("jsonvalidate")
    input <- withr::local_tempfile(fileext = ".json")
    site <- list(pecan_events_version = "0.1.2", site_id = "UREA", events = list(
        list(event_type = "fertilization", date = "2022-01-01", urea_n_kg_m2 = 0.0046)
    ))
    jsonlite::write_json(list(site), input, auto_unbox = TRUE)
    expect_true(validate_events_json(input))
    site$events[[1]]$urea_n_kg_m2 <- -0.001
    # Also provide an accepted legacy field to isolate validation of urea's range.
    site$events[[1]]$nh4_n_kg_m2 <- 0
    jsonlite::write_json(list(site), input, auto_unbox = TRUE)
    expect_false(validate_events_json(input))
})

testthat::test_that("validate_events_json validates good fixtures", {
    f1 <- system.file("events_fixtures/events_site1.json", package = "PEcAn.data.land", mustWork = TRUE)
    f2 <- system.file("events_fixtures/events_site1_site2.json", package = "PEcAn.data.land", mustWork = TRUE)
    testthat::expect_true(PEcAn.data.land::validate_events_json(f1))
    testthat::expect_true(PEcAn.data.land::validate_events_json(f2))
})

testthat::test_that("validate_events_json returns FALSE on invalid JSON", {
    bad <- withr::local_tempfile(fileext = ".json")
    # Missing required field: events
    jsonlite::write_json(list(pecan_events_version = "0.1.0", site_id = "X"), bad, auto_unbox = TRUE)
    testthat::expect_false(PEcAn.data.land::validate_events_json(bad))
})

testthat::test_that("validate_events_json returns NA if jsonvalidate is unavailable", {
    f1 <- system.file("events_fixtures/events_site1.json", package = "PEcAn.data.land", mustWork = TRUE)
    # Use testthat mocking to simulate missing jsonvalidate pkg by overriding base::requireNamespace
    testthat::with_mocked_bindings(
        requireNamespace = function(pkg, quietly = TRUE) {
            if (identical(pkg, "jsonvalidate")) {
                return(FALSE)
            }
            base::requireNamespace(pkg, quietly = quietly)
        },
        {
            testthat::expect_true(is.na(PEcAn.data.land::validate_events_json(f1)))
        },
        .package = "base"
    )
})
