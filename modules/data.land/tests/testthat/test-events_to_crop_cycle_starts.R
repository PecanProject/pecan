test_that("non-planting events are ignored", {
  dat <- dplyr::tribble(
    ~site_id, ~date, ~event_type, ~crop_code,
    "a", "2016-01-01", "planting", "almond",
    "a", "2016-05-01", "irrigation", NA_character_,
    "a", "2017-01-01", "planting", "almond",
    "a", "2017-05-15", "fertilization", NA_character_,
  )
  res <- find_crop_changes(dat)
  expect_equal(nrow(res), 1)
  expect_equal(res$date, "2016-01-01")
  expect_equal(res, find_crop_changes(dat[-c(2, 4), ]))
})

test_that("nonconsecutive runs of the same crop counted separately", {
  dat <- dplyr::tribble(
    ~site_id, ~date, ~event_type, ~crop_code,
    "b", "2016-03-01", "planting", "tomato",
    "b", "2017-03-05", "planting", "tomato",
    "b", "2018-04-15", "planting", "potato",
    "b", "2018-08-01", "planting", "tomato",
  )
  res <- find_crop_changes(dat)
  expect_equal(nrow(res), 3)
  expect_equal(res$date, dat$date[c(1, 3, 4)])
})

test_that("sites are counted separately", {
  dat <- dplyr::tribble(
    ~site_id, ~date, ~event_type, ~crop_code,
    "a", "2016-03-01", "planting", "grape",
    "b", "2016-03-01", "planting", "grape",
    "c", "2023-03-01", "planting", "grape",
  )
  res <- find_crop_changes(dat)
  expect_equal(nrow(res), 3)
  expect_equal(res$date, dat$date)
  expect_equal(res$site_id, dat$site_id)
})

test_that("reads from JSON", {
  path <- system.file(
    "events_fixtures/events_site1.json",
    package = "PEcAn.data.land"
  )
  res <- events_to_crop_cycle_starts(path)
  expect_equal(res$date, as.Date("2022-02-19"))
  expect_equal(res$crop_code, "Mo17")
})
