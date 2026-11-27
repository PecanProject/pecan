
test_that("single.MA returns consistent draw counts after retry", {
  skip_if_not_installed("rjags")
  skip_if_not_installed("mockery")
  skip_on_cran()

  # Check that JAGS is available
  jags_available <- tryCatch({
    rjags::jags.model(
      textConnection("model { x ~ dnorm(0, 1) }"),
      data = list(),
      n.chains = 1,
      quiet = TRUE
    )
    TRUE
  }, error = function(e) FALSE)

  skip_if(!jags_available, "JAGS not available")

  # Prepare realistic data for single.MA()
  raw_data <- create_test_trait_data(
    n_obs = 15,
    trait_mean = 20,
    trait_sd = 2
  )

  jagged_data <- PEcAn.MA::jagify(raw_data)

  # Keep only the columns used by the JAGS model
  jagged_data <- jagged_data[, c(
    "Y", "n", "site", "trt", "ghs", "obs.prec"
  )]

  prior <- create_test_priors("SLA")
  model_file <- tempfile(fileext = ".bug")

  on.exit(unlink(model_file), add = TRUE)

  # Force the convergence check to trigger the retry branch
  gelman_diag_called <- 0

  mockery::stub(
    PEcAn.MA::single.MA,
    "coda::gelman.diag",
    function(...) {
      gelman_diag_called <<- gelman_diag_called + 1
      list(mpsrf = 1.3)
    }
  )

  result <- PEcAn.MA::single.MA(
    data = jagged_data,
    j.chains = 4,
    j.iter = 3000,
    tauA = 1,
    tauB = 1,
    prior = prior,
    jag.model.file = model_file,
    overdispersed = FALSE
  )

  # Verify the result contains four chains
  expect_equal(gelman_diag_called, 1)
  expect_s3_class(result, "mcmc.list")
  expect_length(result, 4)

  # Each chain should contain exactly 1,500 posterior draws
  draws_per_chain <- vapply(result, nrow, integer(1))

  expect_equal(
    unname(draws_per_chain),
    rep(1500L, 4)
  )
})
