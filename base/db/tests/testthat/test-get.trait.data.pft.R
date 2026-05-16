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
    "Could not find pft"
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

test_that("return value does not include trait_data or prior_distns", {
  test_dbcon <- check_db_test()
  withr::defer(PEcAn.DB::db.close(test_dbcon))
  outdir <- withr::local_tempdir()
  result <- get.trait.data.pft(
    pft = make_test_pft(outdir), modeltype = std_modeltype,
    dbfiles = outdir, dbcon = test_dbcon, trait.names = std_traits
  )
  withr::defer(cleanup_posterior(test_dbcon, result$posteriorid))
  expect_false("trait_data"   %in% names(result))
  expect_false("prior_distns" %in% names(result))
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