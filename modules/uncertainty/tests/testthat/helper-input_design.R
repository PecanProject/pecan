# Shared test fixtures for input design tests
# Automatically sourced by testthat before running tests

make_test_settings <- function() {
  list(
    outdir = "/fake/output/path",
    pfts = list(list(name = "pft1")),
    ensemble = list(
      samplingspace = list(
        parameters = list(method = "uniform"),
        met = list(method = "sampling")
      )
    )
  )
}

mock_sa_samples <- list(
  pft1 = structure(
    matrix(1:9, nrow = 3, ncol = 3),
    dimnames = list(c("25", "50", "75"), c("trait1", "trait2", "trait3"))
  )
)
