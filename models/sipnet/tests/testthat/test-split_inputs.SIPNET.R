test_that("split_inputs", {

  climfile <- system.file("niwot.clim", package = "PEcAn.SIPNET")
  outdir <- withr::local_tempdir()

  dates <- seq(
    from = as.Date("1998-11-01"),
    to = as.Date("2005-12-31"),
    by = "2 years"
  )

  clim_split_list <- mapply(
    split_inputs.SIPNET,
    start.time = dates,
    stop.time = c(dates[-1], as.Date("2006-01-01")), # Stop just _before_ these dates
    MoreArgs = list(
      inputs = list(met = list(path = climfile)),
      outpath = outdir
    )
  )
  clim_split <- vapply(clim_split_list, `[[`, character(1), "path")

  # all steps processed
  expect_length(clim_split, 4)
  expect_true(all(file.exists(clim_split)))

  # All lines appear,numerically equal, in exactly 1 split file
  # NB raw text does differ (split_inputs changes some `0.000` to `0`, etc),
  # but should parse equal when read as numeric.
  expect_equal(
    read.table(climfile),
    clim_split |>
      lapply(read.table) |>
      c(make.row.names = FALSE) |>
      do.call(what = rbind)
  )
})

test_that("split_inputs insists on start/end time being dates", {
  outdir <- withr::local_tempdir()
  climfile <- file.path("data", "niwot_1999_v2.clim")

  expect_error(split_inputs.SIPNET(
    start.time = "1999-01-01",
    stop.time = "1999-04-01",
    inputs = list(met = list(path = climfile)),
    outpath = outdir
  ), "Invalid start.time")
})

test_that("v2 clim format", {
  outdir <- withr::local_tempdir()
  climfile <- file.path("data", "niwot_1999_v2.clim")

  clim_split_list <- split_inputs.SIPNET(
    start.time = as.Date("1999-01-01"),
    stop.time = as.Date("1999-04-01"),
    inputs = list(met = list(path = climfile)),
    outpath = outdir
  )
  clim_split <- vapply(clim_split_list, \(x) x$path, character(1))
  expect_length(clim_split, 1)
  expect_length(readLines(clim_split), 90 * 2) # Jan-March 2x/day
  expect_equal(ncol(read.table(clim_split, nrows = 1)), 12)
})

test_that("splitting event files", {
  outdir <- withr::local_tempdir()
  eventfile <- file.path("data", "events-39011.in")
  dates <- seq(
    from = as.Date("2016-01-01"),
    to = as.Date("2024-01-01"),
    by = "6 months"
  )

  inputs <- mapply(
    split_inputs.SIPNET,
    start.time = head(dates, -1),
    stop.time = tail(dates, -1),
    MoreArgs = list(
      inputs = list(events = list(path = eventfile)),
      outpath = outdir
    )
  )

  new_events <- vapply(inputs, `[[`, character(1), "path")
  expect_true(length(new_events) == (length(dates) - 1))
  expect_true(all(file.exists(new_events)))

  original <- readLines(eventfile)
  combined <- lapply(new_events, readLines) |>
    do.call(what = c) |>
    unname()
  expect_equal(original, combined)
})
