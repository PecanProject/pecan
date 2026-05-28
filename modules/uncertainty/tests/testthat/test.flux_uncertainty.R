## Tests for flux.uncertainty()
##
## The bug fixed in this PR:
##   When all flux measurements are negative (all bins fall below zero),
##   the positive-flux linear model `mp` is never assigned.  The original
##   code unconditionally referenced `mp` in the else-branch for slopeN,
##   causing an "object 'mp' not found" error at runtime.
##
##   Fix: the else-branch is now `else if (exists("mp", inherits = FALSE))`
##   so slopeN falls back to the positive model only when it was actually fit.

## Helper: create paired flux measurements where even-indexed values
## (which become the "magnitude" in flux.uncertainty) span a requested range.
make_flux_series <- function(range_lo, range_hi, n_pairs = 80, sd_noise = 0.2, seed = 1) {
  set.seed(seed)
  base <- seq(range_lo, range_hi, length.out = n_pairs)
  # interleave: odd positions = one reading, even positions = paired reading
  meas <- as.vector(rbind(
    base + stats::rnorm(n_pairs, sd = sd_noise),
    base + stats::rnorm(n_pairs, sd = sd_noise)
  ))
  QC <- rep(0L, length(meas))
  list(measurement = meas, QC = QC)
}

test_that("flux.uncertainty returns cleanly when all fluxes are negative (mp never assigned)", {
  d <- make_flux_series(range_lo = -20, range_hi = -2)

  # Before the fix this would throw "object 'mp' not found"
  result <- suppressMessages(
    capture.output(
      val <- flux.uncertainty(d$measurement, QC = d$QC, bin.num = 5, minBin = 3),
      type = "output"
    )
  )

  expect_type(val, "list")
  expect_true(all(c("mag", "err", "bias", "n", "intercept") %in% names(val)),
    label = "required list elements present")
  # No positive bins -> slopeP should not be set
  expect_false("slopeP" %in% names(val),
    label = "slopeP absent when no positive-flux bins exist")
})

test_that("flux.uncertainty fits both slopes for mixed positive/negative flux", {
  d <- make_flux_series(range_lo = -15, range_hi = 15)

  capture.output(
    val <- flux.uncertainty(d$measurement, QC = d$QC, bin.num = 8, minBin = 3),
    type = "output"
  )

  expect_type(val, "list")
  expect_true("slopeP" %in% names(val), label = "slopeP present for mixed data")
  expect_true("slopeN" %in% names(val), label = "slopeN present for mixed data")
  expect_true(is.numeric(val$slopeP) && length(val$slopeP) == 1)
  expect_true(is.numeric(val$slopeN) && length(val$slopeN) == 1)
})

test_that("flux.uncertainty uses positive-slope fallback for slopeN when neg bins all NA", {
  # All positive flux: neg bins will be empty, slopeN should fall back to mp
  d <- make_flux_series(range_lo = 2, range_hi = 20)

  capture.output(
    val <- flux.uncertainty(d$measurement, QC = d$QC, bin.num = 5, minBin = 3),
    type = "output"
  )

  expect_type(val, "list")
  expect_true("slopeP" %in% names(val), label = "slopeP present for all-positive data")
  # slopeN falls back to mp when available
  if ("slopeN" %in% names(val)) {
    expect_equal(val$slopeN, val$slopeP,
      label = "slopeN equals slopeP when neg bins unavailable and mp is the fallback")
  }
})
