# test-run.meta.analysis.pft.R
# Tests for run.meta.analysis.pft() — the "workflow" wrapper
#
# run.meta.analysis.pft() is a thin wrapper around meta_analysis_standalone()
# that also handles:
#   1. Loading trait.data.Rdata and prior.distns.Rdata from pft$outdir
#   2. Writing trait.mcmc.Rdata, post.distns.Rdata, jagged.data.Rdata
#   3. Registering output files in the BETY database via dbfile.insert
#
# Key behavior: when preconditions fail (missing files, missing posteriorid),
# the function calls PEcAn.logger::logger.severe() which throws an error
# via stop(). These tests verify the correct error is raised.

# ---------------------------------------------------------------------------
# Precondition checks (no DB needed)
# ---------------------------------------------------------------------------

test_that("run.meta.analysis.pft errors when trait.data.Rdata is missing", {
  pft_outdir <- file.path(tempdir(), "test-missing-trait-data")
  on.exit(unlink(pft_outdir, recursive = TRUE), add = TRUE)

  pft <- create_test_pft(outdir = pft_outdir, pft_name = "test.pft.missing")

  # Create only prior.distns.Rdata, NOT trait.data.Rdata
  prior.distns <- create_test_priors("SLA")
  save(prior.distns, file = file.path(pft$outdir, "prior.distns.Rdata"))

  expect_error(
    PEcAn.MA:::run.meta.analysis.pft(
      pft = pft,
      iterations = 1000,
      random = TRUE,
      threshold = 1.2,
      dbfiles = tempdir(),
      dbcon = NULL,
      use_ghs = TRUE,
      update = FALSE
    ),
    "Could not find output from get.trait"
  )
})

test_that("run.meta.analysis.pft errors when prior.distns.Rdata is missing", {
  pft_outdir <- file.path(tempdir(), "test-missing-priors")
  on.exit(unlink(pft_outdir, recursive = TRUE), add = TRUE)

  pft <- create_test_pft(outdir = pft_outdir, pft_name = "test.pft.missing.priors")

  # Create only trait.data.Rdata, NOT prior.distns.Rdata
  trait.data <- list(SLA = create_test_trait_data())
  save(trait.data, file = file.path(pft$outdir, "trait.data.Rdata"))

  expect_error(
    PEcAn.MA:::run.meta.analysis.pft(
      pft = pft,
      iterations = 1000,
      random = TRUE,
      threshold = 1.2,
      dbfiles = tempdir(),
      dbcon = NULL,
      use_ghs = TRUE,
      update = FALSE
    ),
    "Could not find output from get.trait"
  )
})

test_that("run.meta.analysis.pft errors when both input files are missing", {
  pft_outdir <- file.path(tempdir(), "test-missing-both")
  on.exit(unlink(pft_outdir, recursive = TRUE), add = TRUE)

  pft <- create_test_pft(outdir = pft_outdir, pft_name = "test.pft.missing.both")

  expect_error(
    PEcAn.MA:::run.meta.analysis.pft(
      pft = pft,
      iterations = 1000,
      random = TRUE,
      threshold = 1.2,
      dbfiles = tempdir(),
      dbcon = NULL,
      use_ghs = TRUE,
      update = FALSE
    ),
    "Could not find output from get.trait"
  )
})

test_that("run.meta.analysis.pft errors when posteriorid is NULL", {
  pft_outdir <- file.path(tempdir(), "test-no-posteriorid")
  on.exit(unlink(pft_outdir, recursive = TRUE), add = TRUE)

  pft <- create_test_pft(
    outdir = pft_outdir,
    pft_name = "test.pft.no.posteriorid",
    posteriorid = NULL
  )

  # Set up valid input files so we pass the file existence check
  setup_trait_files(pft$outdir, trait_names = "SLA")

  expect_error(
    PEcAn.MA:::run.meta.analysis.pft(
      pft = pft,
      iterations = 1000,
      random = TRUE,
      threshold = 1.2,
      dbfiles = tempdir(),
      dbcon = NULL,
      use_ghs = TRUE,
      update = FALSE
    ),
    "Missing posteriorid"
  )
})

# ---------------------------------------------------------------------------
# Skip check: existing results reuse (no DB needed)
# ---------------------------------------------------------------------------

test_that("run.meta.analysis.pft skips re-analysis when output files exist and update=FALSE", {
  pft_outdir <- file.path(tempdir(), "test-skip-existing")
  on.exit(unlink(pft_outdir, recursive = TRUE), add = TRUE)

  pft <- create_test_pft(outdir = pft_outdir, pft_name = "test.pft.skip")

  # Create the INPUT files (precondition)
  setup_trait_files(pft$outdir, trait_names = "SLA")

  # Create the OUTPUT files that signal analysis was already done
  trait.mcmc <- list()
  save(trait.mcmc, file = file.path(pft$outdir, "trait.mcmc.Rdata"))
  post.distns <- data.frame()
  save(post.distns, file = file.path(pft$outdir, "post.distns.Rdata"))

  result <- PEcAn.MA:::run.meta.analysis.pft(
    pft = pft,
    iterations = 1000,
    random = TRUE,
    threshold = 1.2,
    dbfiles = tempdir(),
    dbcon = NULL,
    use_ghs = TRUE,
    update = FALSE
  )

  # When skipping, it returns the pft object unchanged
  expect_type(result, "list")
  expect_equal(result$name, "test.pft.skip")
  expect_equal(result$outdir, pft_outdir)
})

# ---------------------------------------------------------------------------
# Input file loading verification
# ---------------------------------------------------------------------------

test_that("trait.data.Rdata has the structure expected by run.meta.analysis.pft", {
  pft_outdir <- file.path(tempdir(), "test-trait-loading")
  on.exit(unlink(pft_outdir, recursive = TRUE), add = TRUE)

  test_data <- setup_trait_files(
    pft_outdir,
    trait_names = c("SLA", "Vcmax"),
    n_obs = 8
  )

  trait_file <- file.path(pft_outdir, "trait.data.Rdata")
  expect_true(file.exists(trait_file))

  loaded_env <- new.env()
  load(trait_file, envir = loaded_env)
  expect_true("trait.data" %in% ls(loaded_env))
  expect_type(loaded_env$trait.data, "list")
  expect_equal(names(loaded_env$trait.data), c("SLA", "Vcmax"))

  required_cols <- c("name", "mean", "statname", "stat", "greenhouse",
                     "n", "site_id", "specie_id", "citation_id",
                     "cultivar_id", "date", "time", "control")

  for (trait_name in names(loaded_env$trait.data)) {
    df <- loaded_env$trait.data[[trait_name]]
    expect_s3_class(df, "data.frame")
    expect_true(
      all(required_cols %in% names(df)),
      info = paste("Missing columns in trait data for", trait_name)
    )
    expect_equal(nrow(df), 8)
  }
})

test_that("prior.distns.Rdata has the structure expected by run.meta.analysis.pft", {
  pft_outdir <- file.path(tempdir(), "test-prior-loading")
  on.exit(unlink(pft_outdir, recursive = TRUE), add = TRUE)

  test_data <- setup_trait_files(
    pft_outdir,
    trait_names = c("SLA", "Vcmax")
  )

  prior_file <- file.path(pft_outdir, "prior.distns.Rdata")
  expect_true(file.exists(prior_file))

  loaded_env <- new.env()
  load(prior_file, envir = loaded_env)
  expect_true("prior.distns" %in% ls(loaded_env))
  expect_s3_class(loaded_env$prior.distns, "data.frame")
  expect_equal(rownames(loaded_env$prior.distns), c("SLA", "Vcmax"))
  expect_equal(colnames(loaded_env$prior.distns), c("distn", "parama", "paramb", "n"))
  expect_type(loaded_env$prior.distns$distn, "character")
  expect_type(loaded_env$prior.distns$parama, "double")
  expect_type(loaded_env$prior.distns$paramb, "double")
})

# ---------------------------------------------------------------------------
# Type coercion safety
# ---------------------------------------------------------------------------

test_that("run.meta.analysis.pft handles string inputs from XML settings without type errors", {
  # XML settings are often read as character strings.
  # The function coerces random, use_ghs, and threshold before use.
  # We verify coercion works by passing strings through the skip path —
  # if coercion failed, we'd get a type error instead of a normal return.
  pft_outdir <- file.path(tempdir(), "test-string-coercion")
  on.exit(unlink(pft_outdir, recursive = TRUE), add = TRUE)

  pft <- create_test_pft(outdir = pft_outdir, pft_name = "test.coercion")

  # Set up input AND output files so the function takes the skip path
  setup_trait_files(pft$outdir, trait_names = "SLA")
  trait.mcmc <- list()
  save(trait.mcmc, file = file.path(pft$outdir, "trait.mcmc.Rdata"))
  post.distns <- data.frame()
  save(post.distns, file = file.path(pft$outdir, "post.distns.Rdata"))

  # Pass strings instead of proper types — should not cause type errors
  result <- PEcAn.MA:::run.meta.analysis.pft(
    pft = pft,
    iterations = 1000,
    random = "TRUE",
    threshold = "1.2",
    dbfiles = tempdir(),
    dbcon = NULL,
    use_ghs = "TRUE",
    update = FALSE
  )

  # If we reach here, string coercion worked. The function took the skip path.
  expect_type(result, "list")
  expect_equal(result$name, "test.coercion")
})

# ---------------------------------------------------------------------------
# Empty trait data handling
# ---------------------------------------------------------------------------

test_that("run.meta.analysis.pft returns NA when trait.data is empty list", {
  pft_outdir <- file.path(tempdir(), "test-empty-traits")
  on.exit(unlink(pft_outdir, recursive = TRUE), add = TRUE)

  pft <- create_test_pft(outdir = pft_outdir, pft_name = "test.pft.empty")

  # Save an empty trait.data list — this is what happens when a PFT has
  # no trait observations in the database
  trait.data <- list()
  save(trait.data, file = file.path(pft$outdir, "trait.data.Rdata"))
  prior.distns <- create_test_priors("SLA")
  save(prior.distns, file = file.path(pft$outdir, "prior.distns.Rdata"))

  # With empty trait data, function logs "no trait data for PFT" at INFO level
  # and returns NA — no meta-analysis to perform, but not an error condition
  result <- PEcAn.MA:::run.meta.analysis.pft(
    pft = pft,
    iterations = 1000,
    random = TRUE,
    threshold = 1.2,
    dbfiles = tempdir(),
    dbcon = NULL,
    use_ghs = TRUE,
    update = FALSE
  )

  expect_true(is.na(result))
})