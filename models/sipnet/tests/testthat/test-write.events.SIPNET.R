context("write.events.SIPNET")

# Helper to remove excess whitespace
norm <- function(x) gsub("\\s+", " ", trimws(x))

testthat::test_that("write.events.SIPNET produces expected lines", {
    ev_json1 <- system.file(file.path("events_fixtures", "events_site1.json"),
        package = "PEcAn.data.land"
    )
    outdir <- withr::local_tempdir()
    files <- write.events.SIPNET(ev_json1, outdir)
    expect_length(files, 1)
    got <- readLines(files[1])
    expected <- c(
        "2022  35  till  0.2",
        "2022  40  till  0.1",
        "2022  40  irrig  5 1",
        "2022  40  fert   0 0 10",
        "2022  50  plant  10 3 2 5",
        "2022  250 harv   0.1 0 0 0"
    )
    expect_equal(norm(got), norm(expected))
    # TODO determine What's generating the whitespace differences and eliminate use of norm()
})

testthat::test_that("write.events.SIPNET handles multi-site events.json (one file per site)", {
    ev_json2 <- system.file(file.path("events_fixtures", "events_site1_site2.json"),
        package = "PEcAn.data.land"
    )
    outdir <- withr::local_tempdir()
    files <- write.events.SIPNET(ev_json2, outdir)
    testthat::expect_length(files, 2)
    testthat::expect_true(all(file.exists(files)))
    # quick sanity checks for each site's first/last event ordering
    got1 <- readLines(files[grepl("events-S1\\.in$", files)])
    got2 <- readLines(files[grepl("events-S2\\.in$", files)])
    testthat::expect_true(startsWith(norm(got1[1]), "2022 15 till"))
    testthat::expect_true(startsWith(norm(tail(got1, 1)), "2022 244 harv"))
    testthat::expect_true(startsWith(norm(got2[1]), "2022 60 plant"))
    testthat::expect_true(startsWith(norm(tail(got2, 1)), "2022 69 irrig"))
})

testthat::test_that("events are sorted by date", {
    ev_json1 <- system.file(file.path("events_fixtures", "events_site1.json"),
        package = "PEcAn.data.land"
    )
    outdir <- withr::local_tempdir()
    events_orig <- jsonlite::read_json(ev_json1)
    # Shuffle the events
    n_events <- length(events_orig[[1]]$events)
    withr::with_seed(8675309, {
      idx <- sample(n_events)
    })
    events_shuffled <- events_orig
    events_shuffled[[1]]$events <- events_shuffled[[1]]$events[idx]
    shuffled_file <- file.path(outdir, "events.json")
    jsonlite::write_json(events_shuffled, shuffled_file, auto_unbox = TRUE)
    files <- write.events.SIPNET(shuffled_file, outdir)
    expect_length(files, 1)
    got <- readLines(files[1])
    expected <- c(
        "2022  35  till  0.2",
        "2022  40  till  0.1",
        "2022  40  irrig  5 1",
        "2022  40  fert   0 0 10",
        "2022  50  plant  10 3 2 5",
        "2022  250 harv   0.1 0 0 0"
    )
    # Three events in the middle have the same dates, so they won't sort
    # reliably. Just check that the first and last two events are correct.
    expect_equal(norm(head(got, 1)), norm(head(expected, 1)))
    expect_equal(norm(tail(got, 2)), norm(tail(expected, 2)))
})
