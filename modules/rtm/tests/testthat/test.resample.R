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
  "Spectra down-sampling works",
  {
    spec <- spectra(testspec_ACRU[, 1:5], 400:2500)
    new_wl <- seq(400, 1300, 10)
    true_spec <- spec[[new_wl, ]]
    resample_spec <- resample(spec, new_wl, method = "linear")
    expect_equal(true_spec, resample_spec)
  }
)

test_that(
  "Spectra up-sampling works",
  {
    true_spec <- spectra(testspec_ACRU[, 1:5], 400:2500)
    new_wl <- seq(400, 2500, 10)
    lowres_spec <- true_spec[[new_wl, ]]
    resample_spec <- resample(lowres_spec, 400:2500)
    expect_equal(true_spec, resample_spec, tolerance = 0.005)
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
