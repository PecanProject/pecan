test_that("`runModule.run.meta.analysis` throws an error for incorrect input", {
  expect_error(runModule.run.meta.analysis('test'), "only works with Settings or MultiSettings")
})

test_that("`run.meta.analysis` able to call run.meta.analysis.pft for each pft in the input list", {
  mocked_res <- mockery::mock(1, cycle = TRUE)
  mockery::stub(run.meta.analysis, 'run.meta.analysis.pft', mocked_res)
  mockery::stub(run.meta.analysis, 'PEcAn.DB::db.open', 1)
  mockery::stub(run.meta.analysis, 'PEcAn.DB::db.close', 1)
  pfts <- list('ebifarm.salix', 'temperate.coniferous')
  run.meta.analysis(pfts = pfts, iterations = 1, dbfiles = NULL, database = NULL)
  mockery::expect_called(mocked_res, 2)
  args <- mockery::mock_args(mocked_res)
  expect_equal(args[[1]][[1]], "ebifarm.salix")
  expect_equal(args[[2]][[1]], "temperate.coniferous")
})

test_that("`run.meta.analysis.pft` throws an error if it cannot find output from get.trait", {
  pft <- list(outdir = "", name = "ebifarm.salix")
  expect_error(
    run.meta.analysis.pft(pft = pft, iterations = 1, dbfiles = NULL, dbcon = NULL),
    "Could not find output from get.trait"
  )
})

test_that("`run.meta.analysis.pft` throws an error for missing posteriorid", {
  pft <- list(outdir = "test", name = "ebifarm.salix")
  mockery::stub(run.meta.analysis.pft, 'file.exists', TRUE)
  expect_error(
    run.meta.analysis.pft(pft = pft, iterations = 1, dbfiles = NULL, dbcon = NULL, update = TRUE),
    "Missing posteriorid"
  )
})


# Characterization tests for meta_analysis_standalone()
# Verify return structure and file side effects.

test_that("meta_analysis_standalone returns a list with all three expected elements", {
  skip_if_not_installed("coda")

  # Load existing fixture data from the package's test data directory
  data_dir <- system.file("tests", "testthat", "data",
                          package = "PEcAn.MA",
                          mustWork = FALSE)
  if (!nzchar(data_dir) || !dir.exists(data_dir)) {
    data_dir <- file.path(
      getwd(),
      "..", "..", "tests", "testthat", "data"
    )
    # Try a direct relative path (for devtools::test)
    if (!dir.exists(data_dir)) {
      data_dir <- "data"
    }
  }
  skip_if_not(
    dir.exists(data_dir) &&
      file.exists(file.path(data_dir, "trait.mcmc.RData")) &&
      file.exists(file.path(data_dir, "prior.distns.RData")),
    "Fixture data directory not found"
  )

  # Load fixture priors and MCMC
  prior_env <- new.env()
  load(file.path(data_dir, "prior.distns.RData"), envir = prior_env)
  mcmc_env <- new.env()
  load(file.path(data_dir, "trait.mcmc.RData"), envir = mcmc_env)

  priors <- prior_env$prior.distns
  trait.mcmc <- mcmc_env$trait.mcmc

  # Build minimal trait_data from the mcmc fixture (one obs per trait)
  traits_with_mcmc <- names(trait.mcmc)
  trait_data <- list()
  for (trait in traits_with_mcmc) {
    post_mean <- mean(as.matrix(trait.mcmc[[trait]][, "beta.o"]))
    trait_data[[trait]] <- data.frame(
      mean         = post_mean,
      stat         = 1.0,
      n            = 10L,
      statname     = "SD",
      site_id      = 1L,
      greenhouse   = 0L,
      name         = trait,
      treatment_id = 1L,
      control      = 1L,
      specie_id    = 1L,
      citation_id  = 1L,
      cultivar_id  = NA_integer_,
      date         = NA_character_,
      time         = NA_character_,
      stringsAsFactors = FALSE
    )
  }

  outdir <- tempfile("ma_test_")
  dir.create(outdir)
  on.exit(unlink(outdir, recursive = TRUE), add = TRUE)

  result <- meta_analysis_standalone(
    trait_data = trait_data,
    priors     = priors[traits_with_mcmc, , drop = FALSE],
    iterations = 1000,
    outdir     = outdir,
    pft_name   = "test_pft",
    random     = FALSE,
    threshold  = 5.0  # Lenient threshold so nothing is discarded
  )

  # Return should be a list with exactly these three elements
  expect_true(is.list(result))
  expect_named(result, c("trait.mcmc", "post.distns", "jagged.data"),
               ignore.order = TRUE)

  # trait.mcmc: named list of mcmc.list objects
  expect_true(is.list(result$trait.mcmc))
  if (length(result$trait.mcmc) > 0) {
    first_mcmc <- result$trait.mcmc[[1]]
    expect_true(inherits(first_mcmc, "mcmc.list"),
                info = "Each element of trait.mcmc should be a coda::mcmc.list")
    expect_true("beta.o" %in% colnames(as.matrix(first_mcmc)))
  }

  # post.distns: data frame with expected columns
  expect_s3_class(result$post.distns, "data.frame")
  for (col in c("distn", "parama", "paramb")) {
    expect_true(col %in% names(result$post.distns),
                info = paste("post.distns missing column:", col))
  }

  # jagged.data: named list of data frames
  expect_true(is.list(result$jagged.data))
  if (length(result$jagged.data) > 0) {
    first_jag <- result$jagged.data[[1]]
    expect_s3_class(first_jag, "data.frame")
    expect_true("Y" %in% names(first_jag),
                info = "jagified data should have a 'Y' column")
  }
})


# Characterization tests for run.meta.analysis.pft()
# Verify it reads expected upstream files and produces expected downstream files.

test_that("run.meta.analysis.pft reads upstream files and produces downstream files", {
  skip_if_not_installed("coda")
  skip_if_not_installed("mockery")

  # Set up a temp directory with the upstream fixtures
  pft_outdir <- tempfile("rma_pft_test_")
  dbfiles_dir <- tempfile("rma_dbfiles_")
  dir.create(pft_outdir, recursive = TRUE)
  dir.create(dbfiles_dir, recursive = TRUE)
  on.exit(unlink(c(pft_outdir, dbfiles_dir), recursive = TRUE), add = TRUE)

  # Load existing fixture data
  data_dir <- system.file("tests", "testthat", "data",
                          package = "PEcAn.MA",
                          mustWork = FALSE)
  if (!nzchar(data_dir) || !dir.exists(data_dir)) {
    data_dir <- "data"
  }
  skip_if_not(
    dir.exists(data_dir) &&
      file.exists(file.path(data_dir, "trait.mcmc.RData")) &&
      file.exists(file.path(data_dir, "prior.distns.RData")),
    "Fixture data directory not found"
  )

  # Load fixtures to build upstream files
  prior_env <- new.env()
  load(file.path(data_dir, "prior.distns.RData"), envir = prior_env)
  mcmc_env <- new.env()
  load(file.path(data_dir, "trait.mcmc.RData"), envir = mcmc_env)

  # Build trait.data from mcmc fixture (one obs per trait)
  traits_with_mcmc <- names(mcmc_env$trait.mcmc)
  trait.data <- list()
  for (trait in traits_with_mcmc) {
    post_mean <- mean(as.matrix(mcmc_env$trait.mcmc[[trait]][, "beta.o"]))
    trait.data[[trait]] <- data.frame(
      mean         = post_mean,
      stat         = 1.0,
      n            = 10L,
      statname     = "SD",
      site_id      = 1L,
      greenhouse   = 0L,
      name         = trait,
      treatment_id = 1L,
      control      = 1L,
      specie_id    = 1L,
      citation_id  = 1L,
      cultivar_id  = NA_integer_,
      date         = NA_character_,
      time         = NA_character_,
      stringsAsFactors = FALSE
    )
  }

  prior.distns <- prior_env$prior.distns[traits_with_mcmc, , drop = FALSE]

  # Write upstream files (normally produced by get.trait.data.pft)
  save(trait.data, file = file.path(pft_outdir, "trait.data.Rdata"))
  save(prior.distns, file = file.path(pft_outdir, "prior.distns.Rdata"))

  pft <- list(
    name        = "test_pft",
    outdir      = pft_outdir,
    posteriorid = 99999L
  )

  # Stub out DB file registration (we don't have a real DB)
  mockery::stub(run.meta.analysis.pft, "PEcAn.DB::dbfile.insert", TRUE)

  result <- run.meta.analysis.pft(
    pft        = pft,
    iterations = 1000,
    random     = FALSE,
    threshold  = 5.0,  # Lenient
    dbfiles    = dbfiles_dir,
    dbcon      = NULL,
    use_ghs    = TRUE,
    update     = TRUE
  )

  # Verify expected downstream files exist
  expect_true(
    file.exists(file.path(pft_outdir, "trait.mcmc.Rdata")),
    info = "trait.mcmc.Rdata should be created"
  )
  expect_true(
    file.exists(file.path(pft_outdir, "post.distns.MA.Rdata")),
    info = "post.distns.MA.Rdata should be created"
  )
  expect_true(
    file.exists(file.path(pft_outdir, "post.distns.Rdata")),
    info = "post.distns.Rdata (symlink) should be created"
  )
  expect_true(
    file.exists(file.path(pft_outdir, "jagged.data.Rdata")),
    info = "jagged.data.Rdata should be created"
  )

  # Verify file contents have correct object names and types
  mcmc_check <- new.env()
  load(file.path(pft_outdir, "trait.mcmc.Rdata"), envir = mcmc_check)
  expect_true("trait.mcmc" %in% ls(mcmc_check))
  expect_true(is.list(mcmc_check$trait.mcmc))

  pd_check <- new.env()
  load(file.path(pft_outdir, "post.distns.MA.Rdata"), envir = pd_check)
  expect_true("post.distns" %in% ls(pd_check))
  expect_s3_class(pd_check$post.distns, "data.frame")
  expect_true(all(c("distn", "parama", "paramb") %in% names(pd_check$post.distns)))

  jd_check <- new.env()
  load(file.path(pft_outdir, "jagged.data.Rdata"), envir = jd_check)
  expect_true("jagged.data" %in% ls(jd_check))
  expect_true(is.list(jd_check$jagged.data))
})