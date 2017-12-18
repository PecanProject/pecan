library(PEcAnRTM)
library(testthat)
context("Spectra S3 class")

data(testspec, package = "PEcAnRTM")

rawspec1 <- testspec_ACRU[, 1]
rawspec2 <- testspec_ACRU[, 6:8]
rawspec3 <- testspec_ACRU[1:100, 9:10]

spec1 <- spectra(rawspec1, 400:2500)
spec2 <- spectra(rawspec2, 400:2500)
spec3 <- spectra(rawspec3, 401:500)

test_that(
  "Wavelength mismatch throws error",
  {
    expect_error(spectra(rawspec1, 400:1700))
  }
)

wl1 <- 400
i1 <- wl1 - 399
wl2 <- 500:520
i2 <- wl2 - 399

wl1_NA <- 1500:1800
i1_NA <- wl1_NA - 399
spec1[[wl1_NA]] <- NA

test_that(
  "Spectra subsetting and assignment works as expected",
  {
    expect_equivalent(as.numeric(spec1[[wl1]]), rawspec1[i1])
    expect_equivalent(as.numeric(spec2[[wl2]]), rawspec2[i2, ])
    expect_equal(sum(is.na(spec1)), length(wl1_NA))
    expect_true(all(is.na(spec1[[wl1_NA]])))
    expect_true(all(is.na(spec1[i1_NA])))
  }
)

N_col <- sum(ncol(spec1), ncol(spec2), ncol(spec3))
correct_wl <- 400:2500
N_wl <- length(correct_wl)
spec123 <- cbind(spec1, spec2, spec3)

test_that(
  "Combining spectra by wavelength works as expected",
  {
    expect_equal(nrow(spec123), N_wl)
    expect_equal(ncol(spec123), N_col)
    expect_equivalent(spec123[, 1], spec1[, 1])
  }
)

if (interactive()) {
  plot(spec1)
  matplot(spec2)
}

if (interactive()) {
  matplot(sp, lty = "solid", lwd = 2)
}
