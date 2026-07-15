# Tests for generate_joint_ensemble_design
# shared fixtures in helper-input_design.R

# Canned return shapes for the in-memory sampling path, so the tests run
# without real priors, MCMC, or disk I/O.
fake_loaded_posteriors <- function() {
  list(
    pft_names         = list("pft1"),
    prior_distns_list = list(NULL),
    trait_mcmc_list   = list(NULL),
    independent       = TRUE
  )
}

fake_parameter_samples <- function() {
  list(
    trait.samples    = list(pft1 = list(trait1 = 1:5)),
    sa.samples       = list(),
    ensemble.samples = list(pft1 = data.frame(trait1 = 1:5)),
    runs.samples     = list(),
    env.samples      = list()
  )
}


test_that("generate_joint_ensemble_design returns correct structure", {
  settings <- make_test_settings()
  settings$run <- list(inputs = list(met = list(path = c("met1.nc", "met2.nc"))))

  mockery::stub(generate_joint_ensemble_design, "input.ens.gen",
                function(...) list(ids = sample(1:2, 5, replace = TRUE)))
  mockery::stub(generate_joint_ensemble_design, "load_pft_posteriors",
                function(...) fake_loaded_posteriors())
  mockery::stub(generate_joint_ensemble_design, "get_parameter_samples",
                function(...) fake_parameter_samples())

  result <- generate_joint_ensemble_design(settings, ensemble_size = 5)

  expect_true("X" %in% names(result))
  expect_equal(nrow(result$X), 5)
  expect_true("param" %in% names(result$X))
})


test_that("generated samples are returned alongside the design", {
  settings <- make_test_settings()
  settings$run <- list(inputs = list(met = list(path = c("met1.nc", "met2.nc"))))

  mockery::stub(generate_joint_ensemble_design, "input.ens.gen",
                function(...) list(ids = sample(1:2, 5, replace = TRUE)))
  mockery::stub(generate_joint_ensemble_design, "load_pft_posteriors",
                function(...) fake_loaded_posteriors())
  mockery::stub(generate_joint_ensemble_design, "get_parameter_samples",
                function(...) fake_parameter_samples())

  result <- generate_joint_ensemble_design(settings, ensemble_size = 5)

  expect_true("samples" %in% names(result))
  expect_identical(result$samples, fake_parameter_samples())
})


test_that("passed-in samples skip the loader and sampler", {
  settings <- make_test_settings()
  settings$run <- list(inputs = list(met = list(path = c("met1.nc", "met2.nc"))))

  loader <- mockery::mock(fake_loaded_posteriors())
  sampler <- mockery::mock(fake_parameter_samples())
  mockery::stub(generate_joint_ensemble_design, "input.ens.gen",
                function(...) list(ids = sample(1:2, 5, replace = TRUE)))
  mockery::stub(generate_joint_ensemble_design, "load_pft_posteriors", loader)
  mockery::stub(generate_joint_ensemble_design, "get_parameter_samples", sampler)

  established <- fake_parameter_samples()
  result <- generate_joint_ensemble_design(
    settings, ensemble_size = 5, samples = established
  )

  mockery::expect_called(loader, 0)
  mockery::expect_called(sampler, 0)
  expect_identical(result$samples, established)
})


test_that("ensemble design allows variation in non-param columns unlike OAT", {
  settings <- make_test_settings()

  sa_result <- generate_OAT_SA_design(settings, sa_samples = mock_sa_samples)

  settings$run <- list(inputs = list(met = list(path = c("m1.nc", "m2.nc", "m3.nc"))))
  mockery::stub(generate_joint_ensemble_design, "input.ens.gen",
                function(...) list(ids = c(1, 2, 3, 1, 2)))
  mockery::stub(generate_joint_ensemble_design, "load_pft_posteriors",
                function(...) fake_loaded_posteriors())
  mockery::stub(generate_joint_ensemble_design, "get_parameter_samples",
                function(...) fake_parameter_samples())

  ens_result <- generate_joint_ensemble_design(settings, ensemble_size = 5)

  expect_equal(length(unique(sa_result$X$met)), 1)
  expect_true(length(unique(ens_result$X$met)) > 1)
})
