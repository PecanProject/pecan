# Tests for generate_OAT_SA_design
# Shared fixtures (make_test_settings, mock_sa_samples) in helper-input_design.R

# Canned return shapes for the in-memory sampling path, so the migration tests
# run without real priors, MCMC, or disk I/O (mirrors the ensemble generator
# tests). The bundle carries a populated sa.samples so a design can be built.
oat_loaded_posteriors <- function() {
  list(
    pft_names         = list("pft1"),
    prior_distns_list = list(NULL),
    trait_mcmc_list   = list(NULL),
    independent       = TRUE
  )
}

oat_parameter_samples <- function() {
  list(
    trait.samples    = list(pft1 = list(trait1 = 1:3)),
    sa.samples       = mock_sa_samples,
    ensemble.samples = list(),
    runs.samples     = list(),
    env.samples      = list()
  )
}


test_that("generate_OAT_SA_design returns correct structure and run count", {
  settings <- make_test_settings()

  result <- generate_OAT_SA_design(settings, samples = list(sa.samples = mock_sa_samples))

  # 1 median + 3 traits * 2 non-median quantiles = 7
  expect_equal(nrow(result$X), 7)
  expect_true("param" %in% names(result$X))
  expect_true(is.data.frame(result$X))
})


test_that("generate_OAT_SA_design keeps param sequential and non-param constant at 1", {
  settings <- make_test_settings()

  result <- generate_OAT_SA_design(settings, samples = list(sa.samples = mock_sa_samples))

  expect_equal(result$X$param, seq_len(nrow(result$X)))

  # the label columns describe the run rather than selecting an input, so they
  # are not held at 1
  input_cols <- setdiff(names(result$X), c("param", "sa_pft", "sa_trait", "sa_quantile"))
  for (col in input_cols) {
    expect_true(all(result$X[[col]] == 1))
  }
})


test_that("supplied samples are returned alongside the design", {
  settings <- make_test_settings()
  bundle <- oat_parameter_samples()

  result <- generate_OAT_SA_design(settings, samples = bundle)

  expect_true("samples" %in% names(result))
  expect_identical(result$samples, bundle)
})


test_that("parameters are sampled in memory when no samples are supplied", {
  settings <- make_test_settings()
  settings$sensitivity.analysis <- list(quantiles = c(0.25, 0.5, 0.75))

  loader  <- mockery::mock(oat_loaded_posteriors())
  sampler <- mockery::mock(oat_parameter_samples())
  mockery::stub(generate_OAT_SA_design, "load_pft_posteriors", loader)
  mockery::stub(generate_OAT_SA_design, "get_parameter_samples", sampler)

  result <- generate_OAT_SA_design(settings, samples = NULL)

  mockery::expect_called(loader, 1)
  mockery::expect_called(sampler, 1)

  # SA sampling draws quantiles, not an ensemble
  args <- mockery::mock_args(sampler)[[1]]
  expect_false(args$do_ensemble)
  expect_identical(args$sa_quantiles, c(0.25, 0.5, 0.75))

  # design is built from the freshly sampled sa.samples, and returned with them
  expect_equal(nrow(result$X), 7)
  expect_false(is.null(result$samples$sa.samples))
})


test_that("passed-in samples skip the loader and sampler", {
  settings <- make_test_settings()

  loader  <- mockery::mock()
  sampler <- mockery::mock()
  mockery::stub(generate_OAT_SA_design, "load_pft_posteriors", loader)
  mockery::stub(generate_OAT_SA_design, "get_parameter_samples", sampler)

  established <- oat_parameter_samples()
  result <- generate_OAT_SA_design(settings, samples = established)

  mockery::expect_called(loader, 0)
  mockery::expect_called(sampler, 0)
  expect_identical(result$samples, established)
})


test_that("a bundle with empty sa.samples is rejected", {
  settings <- make_test_settings()

  expect_error(
    generate_OAT_SA_design(settings, samples = list(sa.samples = list())),
    "sa.samples"
  )
})

test_that("the SA design comes back as design_matrix, with X kept alongside", {
  settings <- make_test_settings()

  result <- generate_OAT_SA_design(settings, samples = list(sa.samples = mock_sa_samples))

  expect_true("design_matrix" %in% names(result))
  expect_identical(result$design_matrix, result$X)
})

# -- the design describes its own rows ----

test_that("the design says what each run is", {
  settings <- make_test_settings()

  result <- generate_OAT_SA_design(settings, samples = list(sa.samples = mock_sa_samples))
  design <- result$design_matrix

  expect_true(all(c("sa_pft", "sa_trait", "sa_quantile") %in% names(design)))

  # the first run holds everything at its median
  expect_true(is.na(design$sa_pft[1]))
  expect_true(is.na(design$sa_trait[1]))
  expect_equal(design$sa_quantile[1], "50")

  # every run after moves one trait to one of its non-median quantiles,
  # in the order write.sa.configs walks the design
  expect_equal(design$sa_trait[-1], rep(c("trait1", "trait2", "trait3"), each = 2))
  expect_equal(design$sa_quantile[-1], rep(c("25", "75"), times = 3))
  expect_equal(unique(design$sa_pft[-1]), "pft1")
})

test_that("the labels cover every PFT in order and skip env", {
  settings <- make_test_settings()

  sa_samples <- list(
    pft1 = structure(matrix(1:4, nrow = 2, ncol = 2),
                     dimnames = list(c("50", "75"), c("SLA", "Vcmax"))),
    pft2 = structure(matrix(1:2, nrow = 2, ncol = 1),
                     dimnames = list(c("50", "75"), c("SLA"))),
    env  = structure(matrix(1:2, nrow = 2, ncol = 1),
                     dimnames = list(c("50", "75"), c("temp")))
  )

  design <- generate_OAT_SA_design(settings, samples = list(sa.samples = sa_samples))$design_matrix

  # 1 median + (2 traits + 1 trait) at one non-median quantile each
  expect_equal(nrow(design), 4)
  expect_equal(design$sa_pft[-1], c("pft1", "pft1", "pft2"))
  expect_false(any(design$sa_pft %in% "env"))
})


test_that("adding labels leaves the rest of the design alone", {
  settings <- make_test_settings()

  design <- generate_OAT_SA_design(settings, samples = list(sa.samples = mock_sa_samples))$design_matrix

  # same run count and same param/input columns as before the labels existed
  expect_equal(nrow(design), 7)
  expect_equal(design$param, seq_len(7))
  expect_true(all(design$met == 1))
})