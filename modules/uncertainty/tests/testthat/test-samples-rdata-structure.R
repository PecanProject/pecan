skip_if_not_installed("PEcAn.priors")
skip_if_not_installed("PEcAn.utils")
skip_if_not_installed("PEcAn.logger")
skip_if_not_installed("coda")

# samples.Rdata structure
test_that("samples.Rdata contains all 5 expected objects with correct types", {

  outdir <- tempfile("samples_test_")
  dir.create(outdir, recursive = TRUE)
  on.exit(unlink(outdir, recursive = TRUE), add = TRUE)

  pft_outdir <- file.path(outdir, "pft", "test_pft")
  dir.create(pft_outdir, recursive = TRUE)

  # Write upstream fixtures to disk (normally produced by meta-analysis)
  prior.distns <- make_mock_prior_distns(c("SLA", "Vcmax"))
  save(prior.distns, file = file.path(pft_outdir, "prior.distns.Rdata"))

  trait.mcmc <- make_mock_trait_mcmc(c("SLA"), seed = 42)
  save(trait.mcmc, file = file.path(pft_outdir, "trait.mcmc.Rdata"))

  post.distns <- make_mock_post_distns(c("SLA", "Vcmax"))
  save(post.distns, file = file.path(pft_outdir, "post.distns.Rdata"))

  # Call the pure function
  result <- get_parameter_samples(
    pft_names         = "test_pft",
    prior_distns_list = list(prior.distns),
    trait_mcmc_list   = list(trait.mcmc),
    ensemble.size     = 10,
    ens.sample.method = "uniform",
    sa_quantiles      = c(0.025, 0.5, 0.975),
    do_ensemble       = TRUE,
    independent       = TRUE
  )

  # Save to disk the same way get.parameter.samples() does
  ensemble.samples <- result$ensemble.samples
  trait.samples    <- result$trait.samples
  sa.samples       <- result$sa.samples
  runs.samples     <- result$runs.samples
  env.samples      <- result$env.samples
  save(ensemble.samples, trait.samples, sa.samples, runs.samples, env.samples,
       file = file.path(outdir, "samples.Rdata"))

  # Now load it back and verify structure
  samples_file <- file.path(outdir, "samples.Rdata")
  expect_true(file.exists(samples_file))

  env <- new.env()
  load(samples_file, envir = env)

  # All 5 objects must be present
  expected_names <- c("ensemble.samples", "trait.samples", "sa.samples",
                      "runs.samples", "env.samples")
  for (name in expected_names) {
    expect_true(name %in% ls(env),
                info = paste("samples.Rdata missing object:", name))
  }

  # No unexpected objects
  expect_equal(sort(ls(env)), sort(expected_names))
})



# trait.samples structure
test_that("trait.samples is a nested list: PFT -> trait -> numeric vector", {
  priors <- make_mock_prior_distns(c("SLA", "Vcmax"))
  mcmc   <- make_mock_trait_mcmc("SLA", n_samples = 100)

  result <- get_parameter_samples(
    pft_names         = "temperate.Hardwood",
    prior_distns_list = list(priors),
    trait_mcmc_list   = list(mcmc),
    ensemble.size     = 10,
    ens.sample.method = "uniform",
    do_ensemble       = TRUE
  )

  ts <- result$trait.samples
  expect_true(is.list(ts))
  expect_true("temperate.Hardwood" %in% names(ts))

  pft_ts <- ts[["temperate.Hardwood"]]
  expect_true(is.list(pft_ts))
  expect_true("SLA" %in% names(pft_ts))
  expect_true("Vcmax" %in% names(pft_ts))

  # Each trait's samples should be a numeric vector
  expect_true(is.numeric(pft_ts[["SLA"]]))
  expect_true(is.numeric(pft_ts[["Vcmax"]]))
  expect_true(length(pft_ts[["SLA"]]) > 0)
})



# sa.samples structure
test_that("sa.samples has correct matrix structure when quantiles provided", {
  priors <- make_mock_prior_distns(c("SLA", "Vcmax"))

  result <- get_parameter_samples(
    pft_names         = "test_pft",
    prior_distns_list = list(priors),
    trait_mcmc_list   = list(NULL),
    ensemble.size     = 10,
    sa_quantiles      = c(0.025, 0.5, 0.975),
    do_ensemble       = FALSE
  )

  sa <- result$sa.samples
  expect_true(is.list(sa))
  expect_true(length(sa) > 0)

  # SA samples for each PFT should be a named list of matrices
  # keyed by trait, where each matrix has rows = quantiles
  pft_sa <- sa[["test_pft"]]
  expect_true(is.list(pft_sa))
})



# ensemble.samples structure
test_that("ensemble.samples has correct structure", {
  priors <- make_mock_prior_distns(c("SLA"))

  result <- get_parameter_samples(
    pft_names         = "test_pft",
    prior_distns_list = list(priors),
    trait_mcmc_list   = list(NULL),
    ensemble.size     = 20,
    ens.sample.method = "uniform",
    do_ensemble       = TRUE
  )

  ens <- result$ensemble.samples
  expect_true(is.list(ens))
  expect_true(length(ens) > 0)
})



# runs.samples and env.samples structure
test_that("runs.samples and env.samples are empty lists", {
  priors <- make_mock_prior_distns("SLA")

  result <- get_parameter_samples(
    pft_names         = "test_pft",
    prior_distns_list = list(priors),
    trait_mcmc_list   = list(NULL),
    ensemble.size     = 5,
    ens.sample.method = "uniform",
    do_ensemble       = TRUE
  )

  expect_true(is.list(result$runs.samples))
  expect_equal(length(result$runs.samples), 0)

  expect_true(is.list(result$env.samples))
  expect_equal(length(result$env.samples), 0)
})



# Multi-PFT samples.Rdata has all PFTs present
test_that("samples.Rdata for multiple PFTs preserves all PFT entries", {
  priors1 <- make_mock_prior_distns("SLA")
  priors2 <- make_mock_prior_distns("Vcmax")
  mcmc1   <- make_mock_trait_mcmc("SLA", seed = 1)

  result <- get_parameter_samples(
    pft_names         = c("hardwood", "conifer"),
    prior_distns_list = list(priors1, priors2),
    trait_mcmc_list   = list(mcmc1, NULL),
    ensemble.size     = 10,
    ens.sample.method = "uniform",
    do_ensemble       = TRUE
  )

  # trait.samples should have entries for both PFTs
  expect_true("hardwood" %in% names(result$trait.samples))
  expect_true("conifer"  %in% names(result$trait.samples))

  # ensemble.samples is list(pft_data, sampled_indices) from get.ensemble.samples()
  # PFT names live inside the first element
  expect_true("hardwood" %in% names(result$ensemble.samples[[1]]))
  expect_true("conifer"  %in% names(result$ensemble.samples[[1]]))
})
