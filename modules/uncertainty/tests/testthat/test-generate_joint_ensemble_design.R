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

  sa_result <- generate_OAT_SA_design(settings, samples = list(sa.samples = mock_sa_samples))

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

# ------ sobol designs ---------
# sensitivity is a hard dependency (Imports), so these exercise the real
# soboljansen object rather than a stub.

test_that("a sobol design carries its samples", {
  settings <- make_test_settings()
  settings$run <- list(inputs = list(met = list(path = c("met1.nc", "met2.nc"))))

  mockery::stub(generate_joint_ensemble_design, "input.ens.gen",
                function(...) list(ids = sample(1:2, 10, replace = TRUE)))
  mockery::stub(generate_joint_ensemble_design, "load_pft_posteriors",
                function(...) fake_loaded_posteriors())
  mockery::stub(generate_joint_ensemble_design, "get_parameter_samples",
                function(...) fake_parameter_samples())

  result <- generate_joint_ensemble_design(settings, ensemble_size = 5, sobol = TRUE)

  # still the sensitivity object compute_sobol_indices() needs
  expect_true(inherits(result, "soboljansen"))
  expect_true(all(c("X1", "X2", "X") %in% names(result)))

  # and it now carries the samples too, like the non-sobol return
  expect_identical(result$samples, fake_parameter_samples())
})


test_that("sobol samples cover the doubled ensemble", {
  settings <- make_test_settings()
  settings$run <- list(inputs = list(met = list(path = c("met1.nc", "met2.nc"))))

  sampler <- mockery::mock(fake_parameter_samples())
  mockery::stub(generate_joint_ensemble_design, "input.ens.gen",
                function(...) list(ids = sample(1:2, 10, replace = TRUE)))
  mockery::stub(generate_joint_ensemble_design, "load_pft_posteriors",
                function(...) fake_loaded_posteriors())
  mockery::stub(generate_joint_ensemble_design, "get_parameter_samples", sampler)

  result <- generate_joint_ensemble_design(settings, ensemble_size = 5, sobol = TRUE)

  # sobol doubles the ensemble, so the samples must be drawn for the doubled
  # size or the design's param indices would run past the end of them
  args <- mockery::mock_args(sampler)[[1]]
  expect_equal(args$ensemble.size, 10)

  # the sobol matrix has more rows than that, but every param index it uses
  # still falls inside the sampled set
  expect_true(all(result$X$param >= 1))
  expect_true(all(result$X$param <= 10))
})


test_that("passed-in samples are reused for a sobol design", {
  settings <- make_test_settings()
  settings$run <- list(inputs = list(met = list(path = c("met1.nc", "met2.nc"))))

  loader  <- mockery::mock()
  sampler <- mockery::mock()
  mockery::stub(generate_joint_ensemble_design, "input.ens.gen",
                function(...) list(ids = sample(1:2, 10, replace = TRUE)))
  mockery::stub(generate_joint_ensemble_design, "load_pft_posteriors", loader)
  mockery::stub(generate_joint_ensemble_design, "get_parameter_samples", sampler)

  established <- fake_parameter_samples()
  result <- generate_joint_ensemble_design(
    settings, ensemble_size = 5, samples = established, sobol = TRUE
  )

  mockery::expect_called(loader, 0)
  mockery::expect_called(sampler, 0)
  expect_identical(result$samples, established)
})


test_that("attaching samples leaves the sobol object usable by tell()", {
  settings <- make_test_settings()
  settings$run <- list(inputs = list(met = list(path = c("met1.nc", "met2.nc"))))

  mockery::stub(generate_joint_ensemble_design, "input.ens.gen",
                function(...) list(ids = sample(1:2, 10, replace = TRUE)))
  mockery::stub(generate_joint_ensemble_design, "load_pft_posteriors",
                function(...) fake_loaded_posteriors())
  mockery::stub(generate_joint_ensemble_design, "get_parameter_samples",
                function(...) fake_parameter_samples())

  result <- generate_joint_ensemble_design(settings, ensemble_size = 5, sobol = TRUE)

  # compute_sobol_indices() feeds model output back in through tell(); the
  # samples we attached must not disturb that
  told <- sensitivity::tell(result, y = stats::rnorm(nrow(result$X)))

  expect_true(inherits(told, "soboljansen"))
  expect_false(is.null(told$S))
  expect_false(is.null(told$T))
  expect_identical(told$samples, fake_parameter_samples())
})

test_that("the design comes back as design_matrix, with X kept as its older name", {
  settings <- make_test_settings()
  settings$run <- list(inputs = list(met = list(path = c("met1.nc", "met2.nc"))))

  mockery::stub(generate_joint_ensemble_design, "input.ens.gen",
                function(...) list(ids = sample(1:2, 5, replace = TRUE)))
  mockery::stub(generate_joint_ensemble_design, "load_pft_posteriors",
                function(...) fake_loaded_posteriors())
  mockery::stub(generate_joint_ensemble_design, "get_parameter_samples",
                function(...) fake_parameter_samples())

  result <- generate_joint_ensemble_design(settings, ensemble_size = 5)

  expect_true("design_matrix" %in% names(result))
  expect_identical(result$design_matrix, result$X)
})

test_that("a sobol design also carries design_matrix", {
  settings <- make_test_settings()
  settings$run <- list(inputs = list(met = list(path = c("met1.nc", "met2.nc"))))

  mockery::stub(generate_joint_ensemble_design, "input.ens.gen",
                function(...) list(ids = sample(1:2, 10, replace = TRUE)))
  mockery::stub(generate_joint_ensemble_design, "load_pft_posteriors",
                function(...) fake_loaded_posteriors())
  mockery::stub(generate_joint_ensemble_design, "get_parameter_samples",
                function(...) fake_parameter_samples())

  result <- generate_joint_ensemble_design(settings, ensemble_size = 5, sobol = TRUE)

  # sensitivity sets $X itself; design_matrix is the same matrix under the name
  # the non-sobol return uses
  expect_identical(result$design_matrix, result$X)
  expect_true(inherits(result, "soboljansen"))
})

test_that("the design stays the first element of the return", {
  settings <- make_test_settings()
  settings$run <- list(inputs = list(met = list(path = c("met1.nc", "met2.nc"))))

  mockery::stub(generate_joint_ensemble_design, "input.ens.gen",
                function(...) list(ids = sample(1:2, 5, replace = TRUE)))
  mockery::stub(generate_joint_ensemble_design, "load_pft_posteriors",
                function(...) fake_loaded_posteriors())
  mockery::stub(generate_joint_ensemble_design, "get_parameter_samples",
                function(...) fake_parameter_samples())

  result <- generate_joint_ensemble_design(settings, ensemble_size = 5)

  # sda.enkf_MultiSite and sda.enkf_parallel take the design positionally as
  # [[1]], so reordering this list would silently hand them the wrong object
  expect_identical(result[[1]], result$design_matrix)
})