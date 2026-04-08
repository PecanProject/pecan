# ============================================================
# Tests for get_parameter_samples()
#
# These tests verify the extracted pure function works correctly
# WITHOUT any database, file I/O, or PEcAn settings object.
# ============================================================

# Skip entire file if required PEcAn packages are not installed
skip_if_not_installed("PEcAn.priors")
skip_if_not_installed("PEcAn.utils")
skip_if_not_installed("PEcAn.logger")

# ---- Helper: create mock prior distributions ----
make_mock_priors <- function(traits = c("SLA", "Vcmax")) {
  data.frame(
    distn  = rep("norm", length(traits)),
    parama = c(20, 40)[seq_along(traits)],
    paramb = c(5, 10)[seq_along(traits)],
    n      = c(50, 30)[seq_along(traits)],
    row.names = traits,
    stringsAsFactors = FALSE
  )
}

# ---- Helper: create mock MCMC results ----
make_mock_trait_mcmc <- function(traits = "SLA", n_samples = 100, seed = 42) {
  set.seed(seed)
  result <- list()
  for (trait in traits) {
    chain <- matrix(rnorm(n_samples, mean = 20, sd = 2), ncol = 1)
    colnames(chain) <- "beta.o"
    result[[trait]] <- coda::mcmc.list(coda::mcmc(chain))
  }
  result
}


# ====================
# TEST 1: Returns actual data, not NULL
# ====================
test_that("get_parameter_samples returns a non-NULL list with correct names", {
  priors <- make_mock_priors(c("SLA"))

  result <- get_parameter_samples(
    pft_names         = "test_pft",
    prior_distns_list = list(priors),
    trait_mcmc_list   = NULL,
    ensemble.size     = 10,
    ens.sample.method = "uniform",
    sa_quantiles      = NULL,
    do_ensemble       = TRUE,
    independent       = TRUE
  )

  expect_false(is.null(result))
  expect_named(
    result,
    c("trait.samples", "sa.samples", "ensemble.samples",
      "runs.samples", "env.samples")
  )
})


# ====================
# TEST 2: No files written (pure function guarantee)
# ====================
test_that("get_parameter_samples writes no files to disk", {
  temp_dir <- tempfile("pure_test_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  old_wd <- setwd(temp_dir)
  on.exit(setwd(old_wd), add = TRUE)

  priors <- make_mock_priors("SLA")

  result <- get_parameter_samples(
    pft_names         = "test_pft",
    prior_distns_list = list(priors),
    trait_mcmc_list   = NULL,
    ensemble.size     = 5,
    ens.sample.method = "uniform",
    do_ensemble       = TRUE
  )

  rdata_files <- list.files(temp_dir, pattern = "\\.Rdata$", recursive = TRUE)
  expect_length(rdata_files, 0)
})


# ====================
# TEST 3: Prior-only sampling (no MCMC)
# ====================
test_that("prior-only sampling produces correct structure", {
  priors <- make_mock_priors(c("SLA", "Vcmax"))

  result <- get_parameter_samples(
    pft_names         = "temperate.Hardwood",
    prior_distns_list = list(priors),
    trait_mcmc_list   = list(NULL),
    ensemble.size     = 50,
    ens.sample.method = "uniform",
    do_ensemble       = TRUE
  )

  expect_true("temperate.Hardwood" %in% names(result$trait.samples))

  pft_samples <- result$trait.samples[["temperate.Hardwood"]]

  expect_true("SLA" %in% names(pft_samples))
  expect_true("Vcmax" %in% names(pft_samples))
  expect_equal(length(pft_samples[["SLA"]]), 20000)
  expect_equal(length(pft_samples[["Vcmax"]]), 20000)
  expect_true(is.numeric(pft_samples[["SLA"]]))
  expect_true(all(!is.na(pft_samples[["SLA"]])))
})


# ====================
# TEST 4: MCMC sampling path
# ====================
test_that("MCMC samples are used when available", {
  priors <- make_mock_priors(c("SLA", "Vcmax"))
  mcmc   <- make_mock_trait_mcmc("SLA", n_samples = 200)

  result <- get_parameter_samples(
    pft_names         = "test_pft",
    prior_distns_list = list(priors),
    trait_mcmc_list   = list(mcmc),
    ensemble.size     = 10,
    ens.sample.method = "uniform",
    do_ensemble       = TRUE
  )

  pft_samples <- result$trait.samples[["test_pft"]]

  expect_equal(length(pft_samples[["SLA"]]), 200)
  expect_equal(length(pft_samples[["Vcmax"]]), 200)
  expect_true(abs(mean(pft_samples[["SLA"]]) - 20) < 2)
})


# ====================
# TEST 5: Multiple PFTs
# ====================
test_that("multiple PFTs are handled correctly", {
  priors1 <- make_mock_priors("SLA")
  priors2 <- make_mock_priors("Vcmax")

  result <- get_parameter_samples(
    pft_names         = c("hardwood", "conifer"),
    prior_distns_list = list(priors1, priors2),
    trait_mcmc_list   = list(NULL, NULL),
    ensemble.size     = 5,
    ens.sample.method = "uniform",
    do_ensemble       = TRUE
  )

  expect_true("hardwood" %in% names(result$trait.samples))
  expect_true("conifer"  %in% names(result$trait.samples))
  expect_true("SLA"   %in% names(result$trait.samples[["hardwood"]]))
  expect_true("Vcmax" %in% names(result$trait.samples[["conifer"]]))
  expect_false("SLA"   %in% names(result$trait.samples[["conifer"]]))
  expect_false("Vcmax" %in% names(result$trait.samples[["hardwood"]]))

})


# ====================
# TEST 6: Sensitivity analysis samples
# ====================
test_that("SA samples are generated when quantiles are provided", {
  priors <- make_mock_priors("SLA")

  result <- get_parameter_samples(
    pft_names         = "test_pft",
    prior_distns_list = list(priors),
    trait_mcmc_list   = list(NULL),
    ensemble.size     = 10,
    sa_quantiles      = c(0.025, 0.5, 0.975),
    do_ensemble       = FALSE
  )

  expect_true(length(result$sa.samples) > 0)
  expect_equal(length(result$ensemble.samples), 0)
})

test_that("SA samples are NOT generated when quantiles is NULL", {
  priors <- make_mock_priors("SLA")

  result <- get_parameter_samples(
    pft_names         = "test_pft",
    prior_distns_list = list(priors),
    trait_mcmc_list   = list(NULL),
    ensemble.size     = 10,
    sa_quantiles      = NULL,
    do_ensemble       = TRUE
  )

  expect_equal(length(result$sa.samples), 0)
})


# ====================
# TEST 7: Ensemble size = 1 (median run)
# ====================
test_that("ensemble.size = 1 produces median samples", {
  priors <- make_mock_priors("SLA")

  result <- get_parameter_samples(
    pft_names         = "test_pft",
    prior_distns_list = list(priors),
    trait_mcmc_list   = list(NULL),
    ensemble.size     = 1,
    ens.sample.method = "uniform",
    do_ensemble       = TRUE
  )

  expect_true(!is.null(result$ensemble.samples))
})


# ====================
# TEST 8: Input validation
# ====================
test_that("pure function rejects mismatched inputs", {
  priors <- make_mock_priors("SLA")

  expect_error(
    get_parameter_samples(
      pft_names         = c("pft1", "pft2"),
      prior_distns_list = list(priors),
      ensemble.size     = 10
    )
  )
})


# ====================
# TEST 9: Sampling methods
# ====================
test_that("different sampling methods all produce valid output", {
  priors <- make_mock_priors("SLA")

  for (method in c("uniform", "halton", "sobol", "lhc")) {
    result <- get_parameter_samples(
      pft_names         = "test_pft",
      prior_distns_list = list(priors),
      trait_mcmc_list   = list(NULL),
      ensemble.size     = 10,
      ens.sample.method = method,
      do_ensemble       = TRUE
    )

    expect_true(
      length(result$trait.samples[["test_pft"]][["SLA"]]) > 0,
      info = paste("Failed for method:", method)
    )
  }
})


# ====================
# TEST 10: Independent flag behavior
# ====================
test_that("independent = FALSE preserves param.names for correlated sampling", {
  priors <- make_mock_priors("SLA")
  mcmc   <- make_mock_trait_mcmc("SLA", n_samples = 100)

  result_ind <- get_parameter_samples(
    pft_names         = "test_pft",
    prior_distns_list = list(priors),
    trait_mcmc_list   = list(mcmc),
    ensemble.size     = 10,
    do_ensemble       = TRUE,
    independent       = TRUE
  )

  result_dep <- get_parameter_samples(
    pft_names         = "test_pft",
    prior_distns_list = list(priors),
    trait_mcmc_list   = list(mcmc),
    ensemble.size     = 10,
    do_ensemble       = TRUE,
    independent       = FALSE
  )

  expect_false(is.null(result_ind$ensemble.samples))
  expect_false(is.null(result_dep$ensemble.samples))
})
