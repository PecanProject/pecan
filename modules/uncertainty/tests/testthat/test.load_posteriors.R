# Tests for load.posteriors and related helper functions

# Helper: redirect PEcAn.logger to stdout so expect_output() can capture it
setup_logger_for_testing <- function() {
  PEcAn.logger::logger.setUseConsole(TRUE, FALSE)
  PEcAn.logger::logger.setLevel("DEBUG")
}

# Helper: create a coda-style trait.mcmc object (named list of mcmc.list)
make_trait_mcmc <- function(...) {
  traits <- list(...)
  lapply(traits, function(x) {
    coda::mcmc.list(coda::mcmc(matrix(x, ncol = 1, dimnames = list(NULL, "beta.o"))))
  })
}

test_that("load from single distns file works", {
  tmpdir <- withr::local_tempdir()

  # Create a post.distns object and save it
  post.distns <- data.frame(
    distn = c("norm", "norm"),
    parama = c(10, 20),
    paramb = c(2, 5),
    row.names = c("trait_a", "trait_b"),
    stringsAsFactors = FALSE
  )
  save(post.distns, file = file.path(tmpdir, "my_posteriors.Rdata"))

  result <- load.posteriors(
    posterior.file = file.path(tmpdir, "my_posteriors.Rdata")
  )

  expect_null(result$trait.mcmc)
  expect_false(result$is.joint)
  expect_is(result$prior.distns, "data.frame")
  expect_equal(rownames(result$prior.distns), c("trait_a", "trait_b"))
})


test_that("load from single mcmc file works", {
  tmpdir <- withr::local_tempdir()

  # Create a trait.mcmc object using coda classes
  trait.mcmc <- make_trait_mcmc(trait_a = rnorm(100), trait_b = rnorm(100))
  save(trait.mcmc, file = file.path(tmpdir, "samples.Rdata"))

  result <- load.posteriors(
    posterior.file = file.path(tmpdir, "samples.Rdata")
  )

  expect_is(result$trait.mcmc, "list")
  expect_equal(names(result$trait.mcmc), c("trait_a", "trait_b"))
  expect_null(result$prior.distns)
  expect_false(result$is.joint)
})


test_that("load from directory with both mcmc and distns prefers mcmc", {
  tmpdir <- withr::local_tempdir()

  # File 1: distribution summaries
  post.distns <- data.frame(
    distn = c("norm", "norm"),
    parama = c(10, 20),
    paramb = c(2, 5),
    row.names = c("trait_a", "trait_b"),
    stringsAsFactors = FALSE
  )
  save(post.distns, file = file.path(tmpdir, "distributions.Rdata"))

  # File 2: MCMC samples (coda objects)
  trait.mcmc <- make_trait_mcmc(trait_a = rnorm(100))
  save(trait.mcmc, file = file.path(tmpdir, "mcmc_samples.Rdata"))

  result <- load.posteriors(posterior.file = tmpdir)

  # Both should be loaded
  expect_is(result$trait.mcmc, "list")
  expect_equal(names(result$trait.mcmc), c("trait_a"))
  expect_is(result$prior.distns, "data.frame")
  expect_false(result$is.joint)
})


test_that("load from directory with only distns works", {
  tmpdir <- withr::local_tempdir()

  post.distns <- data.frame(
    distn = "norm",
    parama = 10,
    paramb = 2,
    row.names = "trait_a",
    stringsAsFactors = FALSE
  )
  save(post.distns, file = file.path(tmpdir, "post.Rdata"))

  result <- load.posteriors(posterior.file = tmpdir)

  expect_null(result$trait.mcmc)
  expect_is(result$prior.distns, "data.frame")
  expect_equal(rownames(result$prior.distns), "trait_a")
})


test_that("load from directory with only mcmc works", {
  tmpdir <- withr::local_tempdir()

  trait.mcmc <- make_trait_mcmc(trait_a = rnorm(100))
  save(trait.mcmc, file = file.path(tmpdir, "mcmc.Rdata"))

  result <- load.posteriors(posterior.file = tmpdir)

  expect_is(result$trait.mcmc, "list")
  expect_null(result$prior.distns)
})


test_that("fallback to outdir emits deprecation warning", {
  tmpdir <- withr::local_tempdir()
  setup_logger_for_testing()
  on.exit(PEcAn.logger::logger.setUseConsole(TRUE, TRUE), add = TRUE)

  # Create prior.distns in outdir
  prior.distns <- data.frame(
    distn = "norm",
    parama = 10,
    paramb = 2,
    row.names = "trait_a",
    stringsAsFactors = FALSE
  )
  save(prior.distns, file = file.path(tmpdir, "prior.distns.Rdata"))

  expect_output(
    result <- load.posteriors(
      posterior.file = NA,
      outdir = tmpdir
    ),
    "deprecated"
  )

  expect_is(result$prior.distns, "data.frame")
  expect_equal(rownames(result$prior.distns), "trait_a")
})


test_that("fallback to outdir loads post.distns over prior.distns", {
  tmpdir <- withr::local_tempdir()

  # Create both post.distns and prior.distns
  post.distns <- data.frame(
    distn = "norm",
    parama = 99,
    paramb = 1,
    row.names = "trait_post",
    stringsAsFactors = FALSE
  )
  save(post.distns, file = file.path(tmpdir, "post.distns.Rdata"))

  prior.distns <- data.frame(
    distn = "norm",
    parama = 10,
    paramb = 2,
    row.names = "trait_prior",
    stringsAsFactors = FALSE
  )
  save(prior.distns, file = file.path(tmpdir, "prior.distns.Rdata"))

  invisible(capture.output(
    result <- load.posteriors(
      posterior.file = NA,
      outdir = tmpdir
    )
  ))

  # Should get post.distns, not prior.distns
  expect_equal(rownames(result$prior.distns), "trait_post")
})


test_that("fallback outdir finds mcmc in directory scan", {
  tmpdir <- withr::local_tempdir()

  # Create MCMC file with non-standard name (coda objects)
  trait.mcmc <- make_trait_mcmc(trait_a = rnorm(100))
  save(trait.mcmc, file = file.path(tmpdir, "my_custom_mcmc.Rdata"))

  invisible(capture.output(
    result <- load.posteriors(
      posterior.file = NA,
      outdir = tmpdir
    )
  ))

  expect_is(result$trait.mcmc, "list")
  expect_equal(names(result$trait.mcmc), "trait_a")
})


test_that("prior.distns file accepted when post.distns absent", {
  tmpdir <- withr::local_tempdir()

  prior.distns <- data.frame(
    distn = "norm",
    parama = 5,
    paramb = 1,
    row.names = "my_trait",
    stringsAsFactors = FALSE
  )
  save(prior.distns, file = file.path(tmpdir, "priors.Rdata"))

  result <- load.posteriors(posterior.file = file.path(tmpdir, "priors.Rdata"))

  expect_is(result$prior.distns, "data.frame")
  expect_equal(rownames(result$prior.distns), "my_trait")
  expect_null(result$trait.mcmc)
})


test_that("nonexistent path returns empty result with error", {
  tmpdir <- withr::local_tempdir()
  fake_path <- file.path(tmpdir, "does_not_exist.Rdata")
  setup_logger_for_testing()
  on.exit(PEcAn.logger::logger.setUseConsole(TRUE, TRUE), add = TRUE)

  expect_output(
    result <- load.posteriors(
      posterior.file = fake_path
    ),
    "does not exist"
  )

  expect_null(result$prior.distns)
  expect_null(result$trait.mcmc)
  expect_false(result$is.joint)
})


test_that("empty directory returns empty result", {
  tmpdir <- withr::local_tempdir()
  setup_logger_for_testing()
  on.exit(PEcAn.logger::logger.setUseConsole(TRUE, TRUE), add = TRUE)

  expect_output(
    result <- load.posteriors(posterior.file = tmpdir),
    "No .Rdata files found"
  )

  expect_null(result$prior.distns)
  expect_null(result$trait.mcmc)
})


test_that("joint detection works via companion file in from.path", {
  tmpdir <- withr::local_tempdir()

  # Create PDA-style MCMC file
  trait.mcmc <- make_trait_mcmc(trait_a = rnorm(100))
  save(trait.mcmc, file = file.path(tmpdir, "trait.mcmc.pda.temperate_1.Rdata"))

  # Create the companion mcmc.pda.* file that PDA always produces
  params.pft <- list(trait_a = rnorm(100))
  save(params.pft, file = file.path(tmpdir, "mcmc.pda.temperate_1.Rdata"))

  result <- load.posteriors(posterior.file = tmpdir)

  expect_true(result$is.joint)
  expect_is(result$trait.mcmc, "list")
})


test_that("non-PDA mcmc is not detected as joint", {
  tmpdir <- withr::local_tempdir()

  # Create MA-style MCMC file (no companion mcmc.pda.* file)
  trait.mcmc <- make_trait_mcmc(trait_a = rnorm(100))
  save(trait.mcmc, file = file.path(tmpdir, "trait.mcmc.Rdata"))

  result <- load.posteriors(posterior.file = tmpdir)

  expect_false(result$is.joint)
  expect_is(result$trait.mcmc, "list")
})


test_that("joint detection works via filename heuristic in legacy path", {
  tmpdir <- withr::local_tempdir()

  # Create mcmc file with pda in name (legacy path uses filename heuristic)
  trait.mcmc <- make_trait_mcmc(trait_a = rnorm(100))
  save(trait.mcmc, file = file.path(tmpdir, "mcmc.pda.Rdata"))

  invisible(capture.output(
    result <- load.posteriors(
      posterior.file = NA,
      outdir = tmpdir
    )
  ))

  expect_true(result$is.joint)
  expect_is(result$trait.mcmc, "list")
})


test_that("MCMC detected by coda class, not variable name", {
  tmpdir <- withr::local_tempdir()

  # Save mcmc.list under a non-standard variable name
  my_custom_mcmc <- make_trait_mcmc(SLA = rnorm(50))
  save(my_custom_mcmc, file = file.path(tmpdir, "output.Rdata"))

  result <- load.posteriors(posterior.file = file.path(tmpdir, "output.Rdata"))

  # Should be detected by coda class, not by variable name
  expect_is(result$trait.mcmc, "list")
  expect_equal(names(result$trait.mcmc), "SLA")
})
