# Tests for get_parameter_samples()
#
# These tests verify the extracted pure function works correctly
# WITHOUT any database, file I/O, or PEcAn settings object.

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


# TEST 1: Returns actual data, not NULL
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


# TEST 2: No files written (pure function guarantee)
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


# TEST 3: Prior-only sampling (no MCMC)
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


# TEST 4: MCMC sampling path
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


# TEST 5: Multiple PFTs
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


# TEST 6: Sensitivity analysis samples
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


# TEST 7: Ensemble size = 1 (median run)
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


# TEST 8: Input validation
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


# TEST 9: Sampling methods
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


# TEST 10: Independent flag behavior
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


# mixing rows breaks b == -a
make_paired_mcmc <- function(n_samples = 200, shift = 0, seed = 1) {
  set.seed(seed)
  a <- rnorm(n_samples)
  lapply(list(a = a + shift, b = -a - shift), function(x) {
    coda::mcmc.list(coda::mcmc(matrix(x, ncol = 1, dimnames = list(NULL, "beta.o"))))
  })
}

test_that("joint sampling is default and independence is an ensemble request", {
  withr::local_seed(1)
  mcmc <- list(make_paired_mcmc(), make_paired_mcmc(shift = 100))
  joint <- get_parameter_samples(
    pft_names = c("pft1", "pft2"),
    prior_distns_list = list(NULL, NULL),
    trait_mcmc_list = mcmc,
    ensemble.size = 50
  )
  independent <- get_parameter_samples(
    pft_names = c("pft1", "pft2"),
    prior_distns_list = list(NULL, NULL),
    trait_mcmc_list = mcmc,
    ensemble.size = 50,
    independent = TRUE
  )
  for (pft in c("pft1", "pft2")) {
    expect_equal(joint$ensemble.samples[[pft]]$b, -joint$ensemble.samples[[pft]]$a)
    expect_true(all(joint$ensemble.samples[[pft]]$a %in% joint$trait.samples[[pft]]$a))
    expect_false(isTRUE(all.equal(
      independent$ensemble.samples[[pft]]$b, -independent$ensemble.samples[[pft]]$a
    )))
  }
  expect_error(get_parameter_samples(
    pft_names = c("pft1", "pft2"),
    prior_distns_list = list(NULL, NULL),
    trait_mcmc_list = mcmc,
    independent = c(FALSE, TRUE)
  ))
})

test_that("joint PFTs with the same number of rows share a row index", {
  for (method in c("uniform", "random", "halton")) {
    result <- get_parameter_samples(
      pft_names         = c("pft1", "pft2"),
      prior_distns_list = list(NULL, NULL),
      trait_mcmc_list   = list(make_paired_mcmc(), make_paired_mcmc(shift = 100)),
      ensemble.size     = 50,
      ens.sample.method = method,
      do_ensemble       = TRUE,
      independent       = FALSE
    )
    expect_equal(
      result$ensemble.samples$pft2$a,
      result$ensemble.samples$pft1$a + 100,
      info = method
    )
  }
})

test_that("a PFT with nothing to sample does not shift joint rows", {
  result <- get_parameter_samples(
    pft_names         = c("empty_pft", "joint_pft"),
    prior_distns_list = list(NULL, NULL),
    trait_mcmc_list   = list(NULL, make_paired_mcmc()),
    ensemble.size     = 50,
    do_ensemble       = TRUE,
    independent       = FALSE
  )
  joint <- result$ensemble.samples$joint_pft
  expect_equal(joint$b, -joint$a)
})

test_that("joint traits with different numbers of rows fail", {
  mcmc <- make_paired_mcmc()
  mcmc$b <- coda::mcmc.list(coda::mcmc(mcmc$b[[1]][1:150, , drop = FALSE]))
  expect_error(
    get_parameter_samples(
      pft_names         = "joint_pft",
      prior_distns_list = list(NULL),
      trait_mcmc_list   = list(mcmc),
      ensemble.size     = 50,
      do_ensemble       = TRUE,
      independent       = FALSE
    ),
    "same number of rows"
  )
})

test_that("joint singleton and oversized ensembles retain allocation draws", {
  withr::local_seed(1)
  allocation <- list(
    leaf = c(0.45, 0.45, 0.05),
    wood = c(0.45, 0.05, 0.45),
    root = c(0.05, 0.45, 0.45)
  )
  mcmc <- lapply(allocation, function(x) {
    coda::mcmc.list(coda::mcmc(matrix(x, ncol = 1, dimnames = list(NULL, "beta.o"))))
  })
  for (size in c(1, 50)) {
    for (method in c("uniform", "halton")) {
      result <- get_parameter_samples(
        pft_names = "pft",
        prior_distns_list = list(NULL),
        trait_mcmc_list = list(mcmc),
        ensemble.size = size,
        ens.sample.method = method
      )$ensemble.samples$pft
      expect_equal(rowSums(result), rep(0.95, size), info = method)
      expect_equal(nrow(result), size)
    }
  }
})

test_that("distribution-only traits remain marginal beside joint draws", {
  withr::local_seed(1)
  priors <- data.frame(
    distn = "norm", parama = c(0, 0, 20), paramb = c(1, 1, 2), n = 50,
    row.names = c("a", "b", "c")
  )
  result <- get_parameter_samples(
    pft_names = "pft",
    prior_distns_list = list(priors),
    trait_mcmc_list = list(make_paired_mcmc()),
    ensemble.size = 50
  )
  sampled <- result$ensemble.samples$pft
  indices <- attr(result$ensemble.samples, "sampled.indices")$pft
  expect_equal(sampled$b, -sampled$a)
  expect_true(stats::sd(sampled$c) > 0)
  expect_false(identical(indices[, match("a", names(sampled))],
                         indices[, match("c", names(sampled))]))
})

test_that("a distribution-only PFT does not become a joint table", {
  withr::local_seed(1)
  result <- get_parameter_samples(
    pft_names = "pft", prior_distns_list = list(make_mock_priors("SLA")),
    ensemble.size = 20
  )
  expect_false(all(result$ensemble.samples$pft$SLA %in% result$trait.samples$pft$SLA))
})

test_that("the joint selector respects the sampled parameter inventory", {
  withr::local_seed(1)
  mcmc <- make_paired_mcmc()
  priors <- data.frame(distn = "norm", parama = 0, paramb = 1, n = 50,
                       row.names = "a")
  result <- get_parameter_samples(
    pft_names = "pft", prior_distns_list = list(priors),
    trait_mcmc_list = list(mcmc), ensemble.size = 20
  )
  expect_named(result$ensemble.samples$pft, "a")
  expect_true(all(result$ensemble.samples$pft$a %in% mcmc$a[[1]][, "beta.o"]))
})

test_that("joint draws do not consume dimensions of a marginal design", {
  traits <- paste0("trait", seq_len(1112))
  chain <- coda::mcmc.list(coda::mcmc(matrix(
    1:3, ncol = 1, dimnames = list(NULL, "beta.o")
  )))
  mcmc <- stats::setNames(rep(list(chain), length(traits)), traits)
  result <- get_parameter_samples(
    pft_names = "pft", prior_distns_list = list(NULL),
    trait_mcmc_list = list(mcmc), ensemble.size = 5,
    ens.sample.method = "sobol"
  )$ensemble.samples$pft
  expect_named(result, traits)
  expect_true(all(vapply(result, identical, logical(1), result[[1]])))
  expect_true(all(result[[1]] %in% 1:3))
})
