# Tests for get.trait.data.pft()
#

# Helpers

dbdir  <- file.path(tempdir(), "dbfiles")

make_test_pft <- function(outdir) {
  list(
    name        = "temperate.deciduous",
    outdir      = outdir,
    posteriorid = NULL,
    constants   = list()
  )
}

make_empty_pft <- function(outdir) {
  list(
    name        = "soil.ALL",
    outdir      = outdir,
    posteriorid = NULL,
    constants   = list()
  )
}

std_modeltype <- "SIPNET"
std_traits    <- c("SLA", "Vcmax", "leaf_respiration_rate_m2")
std_pft       <- "temperate.deciduous"

cleanup_posterior <- function(dbcon, posteriorid) {
  if (!is.null(posteriorid)) {
    try(DBI::dbExecute(dbcon,
      "DELETE FROM dbfiles WHERE container_type = 'Posterior' AND container_id = $1",
      list(posteriorid)), silent = TRUE)
    try(DBI::dbExecute(dbcon,
      "DELETE FROM posteriors WHERE id = $1",
      list(posteriorid)), silent = TRUE)
  }
}

get_pft <- function(pftname, dbcon) {
  get.trait.data.pft(
    pft         = list(name = pftname, outdir = withr::local_tempdir(),
                       posteriorid = NULL, constants = list()),
    trait.names = "SLA",
    dbfiles     = dbdir,
    modeltype   = NULL,
    dbcon       = dbcon 
  )
}

#  Teardown 

old_log_level <- PEcAn.logger::logger.getLevel()
PEcAn.logger::logger.setLevel("WARN")
teardown({
  unlink(dbdir, recursive = TRUE)
  PEcAn.logger::logger.setLevel(old_log_level)
})

# Input validation (no DB required)

test_that("errors with no arguments", {
  expect_error(get.trait.data.pft())
})

test_that("errors with NULL dbcon", {
  outdir <- withr::local_tempdir()
  expect_error(
    get.trait.data.pft(
      pft         = make_test_pft(outdir),
      modeltype   = std_modeltype,
      dbfiles     = outdir,
      dbcon       = NULL,
      trait.names = std_traits
    )
  )
})

# Error cases

test_that("errors for non-existent PFT name", {
  test_dbcon <- check_db_test()
  withr::defer(PEcAn.DB::db.close(test_dbcon))
  outdir <- withr::local_tempdir()
  expect_error(
    get.trait.data.pft(
      pft       = list(name = "NOTAPFT", outdir = outdir,
                       posteriorid = NULL, constants = list()),
      modeltype = std_modeltype,
      dbfiles   = outdir,
      dbcon     = test_dbcon,
      trait.names = std_traits
    ),
    "PFTs were not found"
  )
})

test_that("errors when multiple PFTs share a name", {
  test_dbcon <- check_db_test()
  withr::defer(PEcAn.DB::db.close(test_dbcon))
  outdir <- withr::local_tempdir()
  multi_exists <- tryCatch({
    n <- DBI::dbGetQuery(test_dbcon,
      "SELECT count(*) AS n FROM pfts WHERE name = 'soil'")$n
    n > 1
  }, error = function(e) FALSE)
  skip_if_not(multi_exists, "Need multiple PFTs named 'soil' to test this case")
  expect_error(
    get.trait.data.pft(
      pft       = list(name = "soil", outdir = outdir,
                       posteriorid = NULL, constants = list()),
      modeltype = NULL,
      dbfiles   = outdir,
      dbcon     = test_dbcon,
      trait.names = "SLA"
    ),
    "Multiple PFTs"
  )
})

# File output

test_that("writes expected Rdata files to outdir", {
  test_dbcon <- check_db_test()
  withr::defer(PEcAn.DB::db.close(test_dbcon))
  outdir <- withr::local_tempdir()
  result <- get.trait.data.pft(
    pft = make_test_pft(outdir), modeltype = std_modeltype,
    dbfiles = outdir, dbcon = test_dbcon, trait.names = std_traits
  )
  withr::defer(cleanup_posterior(test_dbcon, result$posteriorid))
  rdata_files <- list.files(outdir, pattern = "\\.Rdata$")
  expect_true("trait.data.Rdata"   %in% rdata_files)
  expect_true("prior.distns.Rdata" %in% rdata_files)
})

test_that("trait.data.Rdata contains a named list", {
  test_dbcon <- check_db_test()
  withr::defer(PEcAn.DB::db.close(test_dbcon))
  outdir <- withr::local_tempdir()
  result <- get.trait.data.pft(
    pft = make_test_pft(outdir), modeltype = std_modeltype,
    dbfiles = outdir, dbcon = test_dbcon, trait.names = std_traits
  )
  withr::defer(cleanup_posterior(test_dbcon, result$posteriorid))
  env <- new.env(parent = emptyenv())
  load(file.path(outdir, "trait.data.Rdata"), envir = env)
  expect_true(exists("trait.data", envir = env))
  expect_true(is.list(env$trait.data))
})

test_that("prior.distns.Rdata contains a data frame with expected columns", {
  test_dbcon <- check_db_test()
  withr::defer(PEcAn.DB::db.close(test_dbcon))
  outdir <- withr::local_tempdir()
  result <- get.trait.data.pft(
    pft = make_test_pft(outdir), modeltype = std_modeltype,
    dbfiles = outdir, dbcon = test_dbcon, trait.names = std_traits
  )
  withr::defer(cleanup_posterior(test_dbcon, result$posteriorid))
  env <- new.env(parent = emptyenv())
  load(file.path(outdir, "prior.distns.Rdata"), envir = env)
  expect_true(exists("prior.distns", envir = env))
  expect_true(is.data.frame(env$prior.distns))
  expect_true(all(c("distn", "parama", "paramb") %in% colnames(env$prior.distns)))
})

# Return value 

test_that("returns pft list with name, posteriorid, and outdir", {
  test_dbcon <- check_db_test()
  withr::defer(PEcAn.DB::db.close(test_dbcon))
  outdir   <- withr::local_tempdir()
  test_pft <- make_test_pft(outdir)
  result <- get.trait.data.pft(
    pft = test_pft, modeltype = std_modeltype,
    dbfiles = outdir, dbcon = test_dbcon, trait.names = std_traits
  )
  withr::defer(cleanup_posterior(test_dbcon, result$posteriorid))
  expect_equal(result$name, test_pft$name)
  expect_equal(result$outdir, outdir)
  expect_true("posteriorid" %in% names(result))
})

test_that("wrapper attaches trait_data and prior_distns to returned pft", {
  # Documents the Week 1 enhancement: these fields are now returned so that
  # run.meta.analysis.pft() can skip load() calls in a later PR
  test_dbcon <- check_db_test()
  withr::defer(PEcAn.DB::db.close(test_dbcon))
  outdir <- withr::local_tempdir()
  result <- get.trait.data.pft(
    pft = make_test_pft(outdir), modeltype = std_modeltype,
    dbfiles = outdir, dbcon = test_dbcon, trait.names = std_traits, return_data = TRUE
  )
  withr::defer(cleanup_posterior(test_dbcon, result$posteriorid))

  expect_true("trait_data"   %in% names(result))
  expect_true("prior_distns" %in% names(result))

  # The attached objects must match what was saved — no silent divergence
  trait_env <- new.env(parent = emptyenv())
  prior_env <- new.env(parent = emptyenv())
  load(file.path(outdir, "trait.data.Rdata"),   envir = trait_env)
  load(file.path(outdir, "prior.distns.Rdata"), envir = prior_env)
  expect_identical(result$trait_data,   trait_env$trait.data)
  expect_identical(result$prior_distns, prior_env$prior.distns)
})

# PFT with no observations

test_that("PFT with no trait observations returns valid result and writes priors", {
  test_dbcon <- check_db_test()
  withr::defer(PEcAn.DB::db.close(test_dbcon))
  outdir <- withr::local_tempdir()
  soil_exists <- tryCatch({
    nrow(DBI::dbGetQuery(test_dbcon,
      "SELECT 1 FROM pfts WHERE name = 'soil.ALL' LIMIT 1")) > 0
  }, error = function(e) FALSE)
  skip_if_not(soil_exists, "soil.ALL PFT not present in test BETY")
  result <- get.trait.data.pft(
    pft = make_empty_pft(outdir), modeltype = std_modeltype,
    dbfiles = outdir, dbcon = test_dbcon, trait.names = std_traits
  )
  withr::defer(cleanup_posterior(test_dbcon, result$posteriorid))
  expect_true(is.list(result))
  expect_true(file.exists(file.path(outdir, "prior.distns.Rdata")))
})

# End-to-end

test_that("end-to-end: disk files are consistent with returned pft", {
  test_dbcon <- check_db_test()
  withr::defer(PEcAn.DB::db.close(test_dbcon))
  outdir <- withr::local_tempdir()
  result <- get.trait.data.pft(
    pft = make_test_pft(outdir), modeltype = std_modeltype,
    dbfiles = outdir, dbcon = test_dbcon, trait.names = std_traits
  )
  withr::defer(cleanup_posterior(test_dbcon, result$posteriorid))
  trait_env <- new.env(parent = emptyenv())
  prior_env <- new.env(parent = emptyenv())
  load(file.path(outdir, "trait.data.Rdata"), envir = trait_env)
  load(file.path(outdir, "prior.distns.Rdata"), envir = prior_env)
  expect_true(is.list(trait_env$trait.data))
  expect_true(is.data.frame(prior_env$prior.distns))
  expect_equal(result$name, "temperate.deciduous")
  expect_equal(result$outdir, outdir)
})

# Restored: cultivar test (see #1958)

test_that("reference species and cultivar PFTs write traits properly", {
  skip("Disabled until Travis bety contains Pavi_alamo and Pavi_all (#1958)")
  test_dbcon <- check_db_test()
  withr::defer(PEcAn.DB::db.close(test_dbcon))

  pavi_sp    <- get_pft("pavi", test_dbcon)
  expect_equal(pavi_sp$name, "pavi")
  sp_csv <- file.path(dbdir, "posterior", pavi_sp$posteriorid, "species.csv")
  sp_trt <- file.path(dbdir, "posterior", pavi_sp$posteriorid, "trait.data.csv")
  expect_true(file.exists(sp_csv))
  expect_true(file.exists(sp_trt))
  expect_gt(file.info(sp_csv)$size, 40)  # i.e. longer than the 40-char header
  expect_gt(file.info(sp_trt)$size, 215) # ditto 215-char header

  pavi_cv    <- get_pft("Pavi_alamo", test_dbcon)
  expect_equal(pavi_cv$name, "Pavi_alamo")
  cv_csv <- file.path(dbdir, "posterior", pavi_cv$posteriorid, "cultivars.csv")
  cv_trt <- file.path(dbdir, "posterior", pavi_cv$posteriorid, "trait.data.csv")
  expect_true(file.exists(cv_csv))
  expect_true(file.exists(cv_trt))
  expect_gt(file.info(cv_csv)$size, 63)  # cultivar.csv headers are longer
  expect_gt(file.info(cv_trt)$size, 215)

  pavi_allcv <- get_pft("Pavi_all", test_dbcon)
  expect_equal(pavi_allcv$name, "Pavi_all")
  allcv_csv <- file.path(dbdir, "posterior", pavi_allcv$posteriorid, "cultivars.csv")
  allcv_trt <- file.path(dbdir, "posterior", pavi_allcv$posteriorid, "trait.data.csv")
  expect_true(file.exists(allcv_csv))
  expect_true(file.exists(allcv_trt))
  expect_gt(file.info(allcv_csv)$size, 63)
  expect_gt(file.info(allcv_trt)$size, 215)

  expect_gt(file.info(allcv_csv)$size, file.info(cv_csv)$size)
  expect_gt(file.info(allcv_trt)$size, file.info(cv_trt)$size)
})

test_that("wrapper attaches trait_data and prior_distns on the cache-HIT path", {
  # This guards the early-return path: when foundallfiles is TRUE the wrapper
  # copies files from the DB store and returns early.  trait_data and
  # prior_distns must be present on that path too.
  test_dbcon <- check_db_test()
  withr::defer(PEcAn.DB::db.close(test_dbcon))

  outdir1 <- withr::local_tempdir()
  outdir2 <- withr::local_tempdir()

  # First call — populates the cache and the DB posterior record
  result1 <- get.trait.data.pft(
    pft     = make_test_pft(outdir1),
    modeltype   = std_modeltype,
    dbfiles = dbdir,          
    dbcon   = test_dbcon,
    trait.names = std_traits,
    write   = TRUE            
  )
  withr::defer(cleanup_posterior(test_dbcon, result1$posteriorid))

  # Second call — supplies the posteriorid from the first call so the
  # wrapper finds valid files in the DB and takes the cache-hit path.
  cached_pft          <- make_test_pft(outdir2)
  cached_pft$posteriorid <- result1$posteriorid

  result2 <- get.trait.data.pft(
    pft         = cached_pft,
    modeltype   = std_modeltype,
    dbfiles     = outdir2,
    dbcon       = test_dbcon,
    trait.names = std_traits,
    return_data = TRUE
  )

  expect_true("trait_data"   %in% names(result2),
              info = "trait_data missing on cache-hit return path")
  expect_true("prior_distns" %in% names(result2),
              info = "prior_distns missing on cache-hit return path")
  expect_type(result2$trait_data,   "list")
  expect_s3_class(result2$prior_distns, "data.frame")
})

test_that("cache-hit in-memory objects match the files copied to outdir", {
  # Verifies the objects attached on the cache-hit path are identical to
  # what was written to disk on the original (cache-miss) run.
  test_dbcon <- check_db_test()
  withr::defer(PEcAn.DB::db.close(test_dbcon))

  outdir_miss <- withr::local_tempdir()
  outdir_hit  <- withr::local_tempdir()

  result_miss <- get.trait.data.pft(
    pft         = make_test_pft(outdir_miss),
    modeltype   = std_modeltype,
    dbfiles     = dbdir,        
    dbcon       = test_dbcon,
    trait.names = std_traits,
    write       = TRUE            
  )
  withr::defer(cleanup_posterior(test_dbcon, result_miss$posteriorid))

  cached_pft             <- make_test_pft(outdir_hit)
  cached_pft$posteriorid <- result_miss$posteriorid

  result_hit <- get.trait.data.pft(
    pft         = cached_pft,
    modeltype   = std_modeltype,
    dbfiles     = outdir_hit,
    dbcon       = test_dbcon,
    trait.names = std_traits,
    return_data = TRUE
  )

  # Load the files the cache copied to outdir_hit
  trait_env <- new.env(parent = emptyenv())
  prior_env <- new.env(parent = emptyenv())
  load(file.path(outdir_hit, "trait.data.Rdata"),   envir = trait_env)
  load(file.path(outdir_hit, "prior.distns.Rdata"), envir = prior_env)

  expect_identical(result_hit$trait_data,   trait_env$trait.data)
  expect_identical(result_hit$prior_distns, prior_env$prior.distns)
})

test_that("wrapper does NOT attach trait_data/prior_distns by default (legacy)", {
  test_dbcon <- check_db_test()
  withr::defer(PEcAn.DB::db.close(test_dbcon))
  outdir <- withr::local_tempdir()
  result <- get.trait.data.pft(
    pft         = make_test_pft(outdir),
    modeltype   = std_modeltype,
    dbfiles     = outdir,
    dbcon       = test_dbcon,
    trait.names = std_traits
  )
  withr::defer(cleanup_posterior(test_dbcon, result$posteriorid))
  expect_false("trait_data"   %in% names(result))
  expect_false("prior_distns" %in% names(result))
})

test_that("wrapper errors for unknown pft_type, not silent fallback", {
  # Verifies the explicit guard in get.trait.data.pft():
  # an unrecognised pft_type must throw, not silently fall through
  # to the species-query path. We mock query_pfts to return a bad record
  # so no live DB call is needed.
  fake_record <- data.frame(
    id       = 1L,
    pft_type = "weird",
    name     = std_pft,
    stringsAsFactors = FALSE
  )
  mockery::stub(get.trait.data.pft, "query_pfts", fake_record)
  fake_dbcon <- structure(list(), class = c("PostgreSQLConnection",
                                            "DBIConnection"))
  outdir <- withr::local_tempdir()
  expect_error(
    get.trait.data.pft(
      pft         = make_test_pft(outdir),
      modeltype   = std_modeltype,
      dbfiles     = outdir,
      dbcon       = fake_dbcon,
      trait.names = std_traits
    )
  )
})
