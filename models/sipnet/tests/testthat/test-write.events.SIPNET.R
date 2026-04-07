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

testthat::test_that("tillage incorporation fraction written as second param when present", {
    tmp <- withr::local_tempfile(fileext = ".json")
    site <- list(
        pecan_events_version = "0.1.0",
        site_id = "site1",
        events = list(
            list(event_type = "tillage", date = "2022-03-15",
                tillage_eff_0to1 = 0.35, incorporation_frac_0to1 = 0.5),
            list(event_type = "tillage", date = "2022-04-01",
                tillage_eff_0to1 = 0.1)
        )
    )
    jsonlite::write_json(list(site), tmp, auto_unbox = TRUE)
    outdir <- withr::local_tempdir()
    files <- write.events.SIPNET(tmp, outdir)
    got <- readLines(files[1])
    # first tillage has incorporation, second omits it
    expect_equal(norm(got[1]), norm("2022  74  till  0.35 0.5"))
    expect_equal(norm(got[2]), norm("2022  91  till  0.1"))
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
