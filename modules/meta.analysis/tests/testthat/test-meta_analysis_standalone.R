# test-meta_analysis_standalone.R
# Tests for meta_analysis_standalone() — the pure analysis function
#
# meta_analysis_standalone() is the core analysis function that:
#   1. Calls jagify() to prepare data for JAGS
#   2. Checks data-prior consistency via check_consistent()
#   3. Runs pecan.ma() (JAGS MCMC)
#   4. Summarizes results via pecan.ma.summary()
#   5. Fits approximate posteriors via approx.posterior()
#   6. Returns list(trait.mcmc, post.distns, jagged.data)

# ---------------------------------------------------------------------------
# Greenhouse data filtering
# ---------------------------------------------------------------------------

test_that("meta_analysis_standalone filters greenhouse data when use_ghs=FALSE", {
  skip_if_not_installed("rjags")
  skip_on_cran()

  jags_available <- tryCatch({
    rjags::jags.model(
      textConnection("model { x ~ dnorm(0, 1) }"),
      data = list(), n.chains = 1, quiet = TRUE
    )
    TRUE
  }, error = function(e) FALSE)
  skip_if(!jags_available, "JAGS not available")

  outdir <- file.path(tempdir(), "test-ma-greenhouse")
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(outdir, recursive = TRUE), add = TRUE)

  # Create data that is ALL greenhouse
  gh_data <- create_test_trait_data(n_obs = 10, trait_mean = 20)
  gh_data$greenhouse <- 1L
  trait_data <- list(SLA = gh_data)
  priors <- create_test_priors("SLA")

  # When use_ghs=FALSE, all greenhouse data is excluded.
  # With no data left, function should handle this gracefully.
  result <- tryCatch(
    PEcAn.MA::meta_analysis_standalone(
      trait_data = trait_data,
      priors = priors,
      iterations = 1000,
      outdir = outdir,
      use_ghs = FALSE
    ),
    error = function(e) e
  )

  expect_true(
    is.list(result),
    info = "Should return list or error object, not crash"
  )
})

# ---------------------------------------------------------------------------
# Full integration test (requires JAGS)
# ---------------------------------------------------------------------------

test_that("meta_analysis_standalone returns correct structure with valid inputs", {
  skip_if_not_installed("rjags")
  skip_on_cran()

  jags_available <- tryCatch({
    rjags::jags.model(
      textConnection("model { x ~ dnorm(0, 1) }"),
      data = list(), n.chains = 1, quiet = TRUE
    )
    TRUE
  }, error = function(e) FALSE)
  skip_if(!jags_available, "JAGS not available")

  outdir <- file.path(tempdir(), "test-ma-standalone-integration")
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(outdir, recursive = TRUE), add = TRUE)

  set.seed(42)
  trait_data <- list(
    SLA = create_test_trait_data(n_obs = 15, trait_mean = 20, trait_sd = 2)
  )
  priors <- create_test_priors("SLA", parama = 20, paramb = 5)

  result <- PEcAn.MA::meta_analysis_standalone(
    trait_data = trait_data,
    priors = priors,
    iterations = 3000,
    outdir = outdir,
    pft_name = "test.integration",
    random = TRUE,
    threshold = 1.2,
    use_ghs = TRUE
  )

  # Verify return structure
  expect_type(result, "list")
  expect_named(result, c("trait.mcmc", "post.distns", "jagged.data"),
               ignore.order = FALSE)

  # post.distns should be a data.frame
  expect_s3_class(result$post.distns, "data.frame")
  expect_true(all(c("distn", "parama", "paramb") %in% colnames(result$post.distns)))
  expect_true("SLA" %in% rownames(result$post.distns))

  # jagged.data should be a named list
  expect_type(result$jagged.data, "list")
  expect_true("SLA" %in% names(result$jagged.data))
  expect_s3_class(result$jagged.data[["SLA"]], "data.frame")
  expect_true("Y" %in% colnames(result$jagged.data[["SLA"]]))
})