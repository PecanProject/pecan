library(testthat)
library(PEcAnRTM)
context("Resampling functions")

test_that(
  "Univariate resampling works",
  {
    from <- seq(2, 20, 2)
    to <- seq(2, 20)
    values <- from ^ 2
    true_values <- to ^ 2
    resample_values <- resample(values, from, to)
    expect_equal(true_values, resample_values)
  }
)

test_that(
  "Multivariate resampling works",
  {
    m <- matrix(1:20, ncol = 2)
    true_m <- cbind(seq(1, 10, 0.5), seq(11, 20, 0.5))
    resample_m <- resample(m, 1:10, seq(1, 10, 0.5))
    expect_equal(true_m, resample_m)
  }
)

data(testspec)
test_that(
  "Spectra resampling works",
  {
    spec <- spectra(testspec_ACRU[, 1:5], 400:2500)
    new_wl <- seq(400, 1300, 10)
    true_spec <- spec[[new_wl, ]]
    resample_spec <- resample(spec, new_wl)
    expect_equal(true_spec, resample_spec)
  }
)

# This test doesn't work with the PEcAn.logger functions because they mess with 
# the warning output.
if (!requireNamespace("PEcAn.logger")) {
  test_that(
    "Resampling outside range throws warning",
    {
      from <- seq(2, 20, 2)
      to <- seq(1, 30)
      values <- from ^ 2
      expect_warning(resample(values, from, to), "Resampled values .* unreliable")
    }
  )
}
