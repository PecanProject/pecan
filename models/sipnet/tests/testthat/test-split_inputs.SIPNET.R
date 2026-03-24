test_that("split_inputs", {

  climfile <- system.file("niwot.clim", package = "PEcAn.SIPNET")
  outdir <- withr::local_tempdir()

  dates <- seq(
    from = as.Date("1998-11-01"),
    to = as.Date("2005-12-31"),
    by = "2 years"
  )

  clim_split <- mapply(
    split_inputs.SIPNET,
    start.time = dates,
    stop.time = c(dates[-1], as.Date("2006-01-01")), # Stop just _before_ these dates
    MoreArgs = list(
      inputs = climfile,
      outpath = outdir
    )
  )

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
      do.call(what = "rbind")
  )
})

test_that("v2 clim format", {
  outdir <- withr::local_tempdir()
  climfile <- file.path("data", "niwot_1999_v2.clim")

  clim_split <- split_inputs.SIPNET(
    start.time = "1999-01-01",
    stop.time = "1999-04-01",
    inputs = climfile,
    outpath = outdir
  )
  expect_length(clim_split, 1)
  expect_length(readLines(clim_split), 90 * 2) # Jan-March 2x/day
  expect_equal(ncol(read.table(clim_split, nrows = 1)), 12)
})
