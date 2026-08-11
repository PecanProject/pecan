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

# ---------------------------------------------------------------------------
# In-memory input path (Modularity Part 2)
# ---------------------------------------------------------------------------

test_that("run.meta.analysis.pft errors when only trait_data is provided", {
  pft_outdir <- file.path(tempdir(), "test-partial-trait-only")
  on.exit(unlink(pft_outdir, recursive = TRUE), add = TRUE)

  pft <- create_test_pft(outdir = pft_outdir, pft_name = "test.partial.trait")
  trait_data <- list(SLA = create_test_trait_data())

  expect_error(
    PEcAn.MA:::run.meta.analysis.pft(
      pft = pft,
      iterations = 1000,
      random = TRUE,
      threshold = 1.2,
      dbfiles = tempdir(),
      dbcon = NULL,
      use_ghs = TRUE,
      update = FALSE,
      trait_data = trait_data,
      prior_distns = NULL
    ),
    "must both be provided together"
  )
})

test_that("run.meta.analysis.pft errors when only prior_distns is provided", {
  pft_outdir <- file.path(tempdir(), "test-partial-prior-only")
  on.exit(unlink(pft_outdir, recursive = TRUE), add = TRUE)

  pft <- create_test_pft(outdir = pft_outdir, pft_name = "test.partial.prior")
  prior_distns <- create_test_priors("SLA")

  expect_error(
    PEcAn.MA:::run.meta.analysis.pft(
      pft = pft,
      iterations = 1000,
      random = TRUE,
      threshold = 1.2,
      dbfiles = tempdir(),
      dbcon = NULL,
      use_ghs = TRUE,
      update = FALSE,
      trait_data = NULL,
      prior_distns = prior_distns
    ),
    "must both be provided together"
  )
})

test_that("run.meta.analysis.pft skips disk loading when in-memory inputs are provided", {
  # When in-memory inputs are provided, the function must not attempt to read
  # trait.data.Rdata or prior.distns.Rdata. We verify this by ensuring those
  # files do not exist on disk and confirming the function still proceeds to
  # call meta_analysis_standalone() with our in-memory objects (stubbed via
  # mockery to avoid the JAGS dependency in unit tests).
  pft_outdir <- file.path(tempdir(), "test-in-memory-skips-load")
  unlink(pft_outdir, recursive = TRUE)
  dir.create(pft_outdir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(pft_outdir, recursive = TRUE), add = TRUE)

  pft <- create_test_pft(outdir = pft_outdir, pft_name = "test.in.memory.skip")

  trait_data <- list(SLA = create_test_trait_data(n_obs = 10))
  prior_distns <- create_test_priors("SLA")

  # Canned result mimicking what meta_analysis_standalone() returns.
  # Kept structurally simple so save() succeeds without JAGS objects.
  fake_ma_result <- list(
    trait.mcmc  = list(SLA = list()),
    post.distns = data.frame(
      distn = "norm", parama = 20, paramb = 5, n = NA_real_,
      row.names = "SLA", stringsAsFactors = FALSE
    ),
    jagged.data = list(SLA = data.frame(Y = 1:10))
  )

  ma_mock <- mockery::mock(fake_ma_result)
  mockery::stub(run.meta.analysis.pft, "meta_analysis_standalone", ma_mock)
  # Stub DB registration so we don't need a real BETY connection
  mockery::stub(run.meta.analysis.pft, "PEcAn.DB::dbfile.insert", NULL)

  result <- run.meta.analysis.pft(
    pft = pft,
    iterations = 100,
    random = TRUE,
    threshold = 1.2,
    dbfiles = tempdir(),
    dbcon = NULL,
    use_ghs = TRUE,
    update = FALSE,
    trait_data = trait_data,
    prior_distns = prior_distns,
    return_data = TRUE
  )

  # meta_analysis_standalone should be called exactly once with our objects
  mockery::expect_called(ma_mock, 1)
  call_args <- mockery::mock_args(ma_mock)[[1]]
  expect_identical(call_args$trait_data, trait_data)
  expect_identical(call_args$priors, prior_distns)

  # Return contract: pft with trait.mcmc, post.distns, jagged.data attached
  expect_type(result, "list")
  expect_equal(result$name, "test.in.memory.skip")
  expect_identical(result$trait.mcmc,  fake_ma_result$trait.mcmc)
  expect_identical(result$post.distns, fake_ma_result$post.distns)
  expect_identical(result$jagged.data, fake_ma_result$jagged.data)

  # Provenance side-effects: wrapper still writes .Rdata files for audit trail
  expect_true(file.exists(file.path(pft_outdir, "trait.mcmc.Rdata")))
  expect_true(file.exists(file.path(pft_outdir, "post.distns.Rdata")))
  expect_true(file.exists(file.path(pft_outdir, "post.distns.MA.Rdata")))
  expect_true(file.exists(file.path(pft_outdir, "jagged.data.Rdata")))
})

test_that("run.meta.analysis.pft returns NA when in-memory trait_data is empty", {
  pft_outdir <- file.path(tempdir(), "test-in-memory-empty")
  unlink(pft_outdir, recursive = TRUE)
  dir.create(pft_outdir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(pft_outdir, recursive = TRUE), add = TRUE)

  pft <- create_test_pft(outdir = pft_outdir, pft_name = "test.in.memory.empty")

  # Mirrors the disk-path "no observations" branch: when length(trait_data) == 0
  # the function logs an info message and returns NA without running JAGS.
  result <- PEcAn.MA:::run.meta.analysis.pft(
    pft = pft,
    iterations = 100,
    random = TRUE,
    threshold = 1.2,
    dbfiles = tempdir(),
    dbcon = NULL,
    use_ghs = TRUE,
    update = FALSE,
    trait_data = list(),
    prior_distns = create_test_priors("SLA")
  )

  expect_true(is.na(result))
})

test_that("run.meta.analysis.pft skip path attaches existing results to returned pft", {
  pft_outdir <- file.path(tempdir(), "test-skip-attaches-results")
  unlink(pft_outdir, recursive = TRUE)
  dir.create(pft_outdir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(pft_outdir, recursive = TRUE), add = TRUE)

  pft <- create_test_pft(outdir = pft_outdir, pft_name = "test.skip.attaches")

  # Set up input files (precondition) plus the output files that trigger the
  # skip path. We use recognizable sentinel values to verify they're loaded
  # and attached, not silently dropped.
  setup_trait_files(pft$outdir, trait_names = "SLA")

  trait.mcmc <- list(SLA = "sentinel-mcmc")
  save(trait.mcmc, file = file.path(pft$outdir, "trait.mcmc.Rdata"))
  post.distns <- data.frame(
    distn = "norm", parama = 99, paramb = 99, n = NA_real_,
    row.names = "SLA", stringsAsFactors = FALSE
  )
  save(post.distns, file = file.path(pft$outdir, "post.distns.Rdata"))
  jagged.data <- list(SLA = "sentinel-jagged")
  save(jagged.data, file = file.path(pft$outdir, "jagged.data.Rdata"))

  result <- PEcAn.MA:::run.meta.analysis.pft(
    pft = pft,
    iterations = 1000,
    random = TRUE,
    threshold = 1.2,
    dbfiles = tempdir(),
    dbcon = NULL,
    use_ghs = TRUE,
    update = FALSE,
    return_data = TRUE
  )

  expect_equal(result$name, "test.skip.attaches")
  expect_equal(result$trait.mcmc,  list(SLA = "sentinel-mcmc"))
  expect_equal(result$post.distns$parama, 99)
  expect_equal(result$jagged.data, list(SLA = "sentinel-jagged"))
})

test_that("run.meta.analysis.pft skip path tolerates missing jagged.data.Rdata", {
  # Older posteriors copied via get.trait.data.pft may not include
  # jagged.data.Rdata. The skip path should still succeed and return a pft
  # with trait.mcmc and post.distns attached.
  pft_outdir <- file.path(tempdir(), "test-skip-no-jagged")
  unlink(pft_outdir, recursive = TRUE)
  dir.create(pft_outdir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(pft_outdir, recursive = TRUE), add = TRUE)

  pft <- create_test_pft(outdir = pft_outdir, pft_name = "test.skip.no.jagged")

  setup_trait_files(pft$outdir, trait_names = "SLA")
  trait.mcmc <- list()
  save(trait.mcmc, file = file.path(pft$outdir, "trait.mcmc.Rdata"))
  post.distns <- data.frame()
  save(post.distns, file = file.path(pft$outdir, "post.distns.Rdata"))
  # Deliberately do NOT save jagged.data.Rdata

  result <- PEcAn.MA:::run.meta.analysis.pft(
    pft = pft,
    iterations = 1000,
    random = TRUE,
    threshold = 1.2,
    dbfiles = tempdir(),
    dbcon = NULL,
    use_ghs = TRUE,
    update = FALSE,
    return_data = TRUE
  )

  expect_type(result, "list")
  expect_equal(result$name, "test.skip.no.jagged")
  expect_true("trait.mcmc" %in% names(result))
  expect_true("post.distns" %in% names(result))
  expect_false("jagged.data" %in% names(result))
})

test_that("run.meta.analysis.pft does NOT attach results by default (return_data = FALSE)", {
  # Guards the serialization-safe default: a pft folded back into a settings
  # object must not gain large mcmc/jagged fields unless the caller opts in.
  pft_outdir <- file.path(tempdir(), "test-no-attach-default")
  unlink(pft_outdir, recursive = TRUE)
  dir.create(pft_outdir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(pft_outdir, recursive = TRUE), add = TRUE)

  pft <- create_test_pft(outdir = pft_outdir, pft_name = "test.no.attach")

  # Input + output files present so the function takes the skip path.
  setup_trait_files(pft$outdir, trait_names = "SLA")
  trait.mcmc <- list(SLA = "x")
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

  # Default return_data = FALSE: bare pft, no attached MA outputs.
  expect_false("trait.mcmc"  %in% names(result))
  expect_false("post.distns" %in% names(result))
  expect_false("jagged.data" %in% names(result))
})