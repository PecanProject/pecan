# Tests for get_trait_data_pft()

old_log_level <- PEcAn.logger::logger.getLevel()
PEcAn.logger::logger.setLevel("WARN")
teardown({
  PEcAn.logger::logger.setLevel(old_log_level)
})

std_pft       <- "temperate.deciduous"
std_modeltype <- "SIPNET"
std_traits    <- c("SLA", "Vcmax", "leaf_respiration_rate_m2")

# Input validation — no DB connection needed for these

test_that("errors when pft_name is not a single string", {
  # pft_name is validated before dbcon, so list() is safe to pass here
  expect_error(
    get_trait_data_pft(
      pft_name    = c("pft1", "pft2"),
      modeltype   = std_modeltype,
      dbcon       = list(),
      trait_names = "SLA"
    )
  )
})

test_that("errors when trait_names is empty", {
  # trait_names is validated before dbcon, so list() is safe to pass here
  expect_error(
    get_trait_data_pft(
      pft_name    = std_pft,
      modeltype   = std_modeltype,
      dbcon       = list(),
      trait_names = character(0)
    )
  )
})

test_that("errors when dbcon is not a database connection", {
  expect_error(
    get_trait_data_pft(
      pft_name    = std_pft,
      modeltype   = std_modeltype,
      dbcon       = list(),
      trait_names = "SLA"
    )
  )
})

# Database-backed tests

test_that("errors for a PFT name not in the database", {
  test_dbcon <- check_db_test()
  withr::defer(PEcAn.DB::db.close(test_dbcon))

  expect_error(
    get_trait_data_pft(
      pft_name    = "NOTAPFT_GSOC2026",
      modeltype   = std_modeltype,
      dbcon       = test_dbcon,
      trait_names = "SLA"
    )
  )
})

test_that("returns a named list with trait_data, prior_distns, pft_info", {
  test_dbcon <- check_db_test()
  withr::defer(PEcAn.DB::db.close(test_dbcon))

  result <- get_trait_data_pft(
    pft_name    = std_pft,
    modeltype   = std_modeltype,
    dbcon       = test_dbcon,
    trait_names = std_traits
  )

  expect_named(result, c("trait_data", "prior_distns", "pft_info"))
})

test_that("trait_data is a named list of data frames", {
  test_dbcon <- check_db_test()
  withr::defer(PEcAn.DB::db.close(test_dbcon))

  result <- get_trait_data_pft(
    pft_name    = std_pft,
    modeltype   = std_modeltype,
    dbcon       = test_dbcon,
    trait_names = std_traits
  )

  expect_type(result$trait_data, "list")
  for (df in result$trait_data) {
    expect_s3_class(df, "data.frame")
    # These columns are the contract that meta_analysis_standalone expects
    expect_true(all(c("mean", "stat", "statname", "n", "site_id",
                       "greenhouse", "specie_id", "citation_id") %in%
                      names(df)))
  }
})

test_that("prior_distns is a data frame with the required columns", {
  test_dbcon <- check_db_test()
  withr::defer(PEcAn.DB::db.close(test_dbcon))

  result <- get_trait_data_pft(
    pft_name    = std_pft,
    modeltype   = std_modeltype,
    dbcon       = test_dbcon,
    trait_names = std_traits
  )

  expect_s3_class(result$prior_distns, "data.frame")
  expect_true(all(c("distn", "parama", "paramb") %in%
                    colnames(result$prior_distns)))
  # Row names are trait names — the contract downstream functions rely on
  expect_true(all(rownames(result$prior_distns) %in% std_traits))
})

test_that("pft_info contains expected fields and posteriorid is NULL", {
  test_dbcon <- check_db_test()
  withr::defer(PEcAn.DB::db.close(test_dbcon))
  result <- get_trait_data_pft(
    pft_name    = std_pft,
    modeltype   = std_modeltype,
    dbcon       = test_dbcon,
    trait_names = "SLA"
  )
  expect_named(
    result$pft_info,
    c("name", "pft_id", "pft_type", "pft_members", "pft_member_filename",
      "posteriorid"),
    ignore.order = TRUE
  )
  expect_equal(result$pft_info$name, std_pft)
  # posteriorid is always NULL here — the wrapper sets it after DB registration
  expect_null(result$pft_info$posteriorid)
  # pft_members must be a data frame with at least an id column
  expect_s3_class(result$pft_info$pft_members, "data.frame")
  expect_true("id" %in% names(result$pft_info$pft_members))
  # pft_member_filename must be species.csv or cultivars.csv
  expect_true(
    result$pft_info$pft_member_filename %in% c("species.csv", "cultivars.csv")
  )
})

test_that("no files are written to disk", {
  test_dbcon <- check_db_test()
  withr::defer(PEcAn.DB::db.close(test_dbcon))

  tmp    <- withr::local_tempdir()
  old_wd <- setwd(tmp)
  withr::defer(setwd(old_wd))

  get_trait_data_pft(
    pft_name    = std_pft,
    modeltype   = std_modeltype,
    dbcon       = test_dbcon,
    trait_names = std_traits
  )

  expect_length(list.files(tmp, pattern = "\\.Rdata$", recursive = TRUE), 0L)
})

test_that("constants are excluded from prior_distns", {
  test_dbcon <- check_db_test()
  withr::defer(PEcAn.DB::db.close(test_dbcon))

  # Get the full prior list first so we know which traits actually have priors
  result_full <- get_trait_data_pft(
    pft_name    = std_pft,
    modeltype   = std_modeltype,
    dbcon       = test_dbcon,
    trait_names = std_traits
  )

  traits_with_priors <- rownames(result_full$prior_distns)
  skip_if(length(traits_with_priors) < 2L,
          "Need at least 2 traits with priors to test constants exclusion")

  constant_trait <- traits_with_priors[[1L]]

  result_const <- get_trait_data_pft(
    pft_name    = std_pft,
    modeltype   = std_modeltype,
    dbcon       = test_dbcon,
    trait_names = std_traits,
    constants   = stats::setNames(list(1.0), constant_trait)
  )

  expect_false(constant_trait %in% rownames(result_const$prior_distns))
  expect_equal(nrow(result_const$prior_distns),
               nrow(result_full$prior_distns) - 1L)
})

test_that("end-to-end: standalone gives identical objects to what wrapper saves", {
  test_dbcon <- check_db_test()
  withr::defer(PEcAn.DB::db.close(test_dbcon))

  outdir   <- withr::local_tempdir()
  test_pft <- list(
    name        = std_pft,
    outdir      = outdir,
    posteriorid = NULL,
    constants   = list()
  )

  # Run the wrapper — it writes trait.data.Rdata and prior.distns.Rdata
  wrapper_result <- get.trait.data.pft(
    pft         = test_pft,
    modeltype   = std_modeltype,
    dbfiles     = outdir,
    dbcon       = test_dbcon,
    trait.names = std_traits
  )
  withr::defer({
    if (!is.null(wrapper_result$posteriorid)) {
      try(DBI::dbExecute(test_dbcon,
        "DELETE FROM dbfiles WHERE container_type = 'Posterior' AND container_id = $1",
        list(wrapper_result$posteriorid)), silent = TRUE)
      try(DBI::dbExecute(test_dbcon,
        "DELETE FROM posteriors WHERE id = $1",
        list(wrapper_result$posteriorid)), silent = TRUE)
    }
  })

  standalone_result <- get_trait_data_pft(
    pft_name    = std_pft,
    modeltype   = std_modeltype,
    dbcon       = test_dbcon,
    trait_names = std_traits
  )

  # Load the on-disk files the wrapper created
  trait_env <- new.env(parent = emptyenv())
  prior_env <- new.env(parent = emptyenv())
  load(file.path(outdir, "trait.data.Rdata"),   envir = trait_env)
  load(file.path(outdir, "prior.distns.Rdata"), envir = prior_env)

  expect_identical(standalone_result$trait_data,   trait_env$trait.data)
  expect_identical(standalone_result$prior_distns, prior_env$prior.distns)
})

test_that("errors for unknown pft_type returns an error, not silent fallback", {
  # This verifies the explicit guard added in get_trait_data_pft():
  # an unrecognised pft_type must throw, not silently fall through
  # to the species-query path.  We simulate a bad record by mocking
  # query_pfts — no live DB call needed.
  fake_record <- data.frame(id = 1L, pft_type = "unknown_type",
                            name = std_pft, stringsAsFactors = FALSE)
  mockery::stub(get_trait_data_pft, "query_pfts", fake_record)

  fake_dbcon <- structure(list(), class = c("PostgreSQLConnection",
                                            "DBIConnection"))
  expect_error(
    get_trait_data_pft(
      pft_name    = std_pft,
      modeltype   = std_modeltype,
      dbcon       = fake_dbcon,
      trait_names = "SLA"
    )
  )
})

test_that("errors when query_pfts returns zero rows", {
  empty_record <- data.frame(
    id       = integer(0),
    pft_type = character(0),
    name     = character(0)
  )
  mockery::stub(get_trait_data_pft, "query_pfts", empty_record)

  fake_dbcon <- structure(list(), class = c("PostgreSQLConnection",
                                            "DBIConnection"))
  expect_error(
    get_trait_data_pft(
      pft_name    = "DoesNotExist",
      modeltype   = "SIPNET",
      dbcon       = fake_dbcon,
      trait_names = "SLA"
    ),
    "PFTs were not found"
  )
})

test_that("errors when query_pfts returns multiple rows (multi-modeltype case)", {
  multi_record <- data.frame(
    id       = c(1L, 2L),
    pft_type = c("plant", "plant"),
    name     = c("temperate.deciduous", "temperate.deciduous"),
    stringsAsFactors = FALSE
  )
  mockery::stub(get_trait_data_pft, "query_pfts", multi_record)

  fake_dbcon <- structure(list(), class = c("PostgreSQLConnection",
                                            "DBIConnection"))
  expect_error(
    get_trait_data_pft(
      pft_name    = "temperate.deciduous",
      modeltype   = NULL,
      dbcon       = fake_dbcon,
      trait_names = "SLA"
    ),
    "Multiple PFTs"
  )
})

test_that("errors when query_pfts returns zero rows", {
  empty_record <- data.frame(
    id       = integer(0),
    pft_type = character(0),
    name     = character(0)
  )
  mockery::stub(get_trait_data_pft, "query_pfts", empty_record)

  fake_dbcon <- structure(list(), class = c("PostgreSQLConnection",
                                            "DBIConnection"))
  expect_error(
    get_trait_data_pft(
      pft_name    = "DoesNotExist",
      modeltype   = "SIPNET",
      dbcon       = fake_dbcon,
      trait_names = "SLA"
    ),
    "PFTs were not found"
  )
})
