# Integration test: modular trait -> meta-analysis -> parameter-sampling chain
#
# Runs the in-memory (Layer 1) pipeline end to end against a live BETYdb and
# checks only that each step's output is accepted by the next without error and
# keeps its documented shape. It does not check whether the values are correct
# or scientifically sensible -- that is the job of the per-package unit tests,
# not of an integration test.
#
# Executed via `Rscript` from .github/workflows/integration-test.yml, inside
# pecan/base:develop (all PEcAn packages plus JAGS installed) with a BETYdb
# stood up the same way .github/workflows/test.yml does it.

library(testthat)
library(PEcAn.DB)
library(PEcAn.MA)
library(PEcAn.uncertainty)

test_modular_chain <- function(pft_name, modeltype, trait_names,
                               iterations    = 3000,
                               ensemble.size = 10) {

  PEcAn.logger::logger.setUseConsole(TRUE, FALSE)
  on.exit(PEcAn.logger::logger.setUseConsole(TRUE, TRUE), add = TRUE)
  PEcAn.logger::logger.setLevel("DEBUG")

  # Connection details come from the Postgres env vars set by the workflow
  # (PGHOST = postgres); the bety/bety/bety defaults match the pecan/db:ci
  # image. Override these env vars to point at a different BETY when running
  # the script locally.
  con <- PEcAn.DB::db.open(list(
    driver   = "Postgres",
    host     = Sys.getenv("PGHOST", "postgres"),
    user     = Sys.getenv("PGUSER", "bety"),
    password = Sys.getenv("PGPASSWORD", "bety"),
    dbname   = Sys.getenv("PGDATABASE", "bety")
  ))
  on.exit(PEcAn.DB::db.close(con), add = TRUE)

  withr::with_dir(tempdir(), {

    # ---- Step 1: trait retrieval (queries BETYdb, returns objects) ----
    trait_step <- PEcAn.DB::get_trait_data_pft(
      pft_name    = pft_name,
      modeltype   = modeltype,
      dbcon       = con,
      trait_names = trait_names
    )

    test_that("get_trait_data_pft returns the documented structure", {
      expect_named(trait_step, c("trait_data", "prior_distns", "pft_info"),
                   ignore.order = TRUE)
      expect_type(trait_step$trait_data, "list")
      expect_s3_class(trait_step$prior_distns, "data.frame")
    })

    # ---- Step 2: meta-analysis consumes the trait-step output ----
    ma <- PEcAn.MA::meta_analysis_standalone(
      trait_data = trait_step$trait_data,
      priors     = trait_step$prior_distns,
      iterations = iterations,
      pft_name   = pft_name
    )

    test_that("meta_analysis_standalone consumes trait output and returns MA results", {
      expect_named(ma, c("trait.mcmc", "post.distns", "jagged.data"),
                   ignore.order = TRUE)
      expect_s3_class(ma$post.distns, "data.frame")
      expect_type(ma$trait.mcmc, "list")
    })

    # ---- Step 3: parameter sampling consumes the meta-analysis output ----
    # The pure get_parameter_samples() takes the posterior distributions in
    # prior_distns_list and the MCMC chains in trait_mcmc_list.
    samples <- PEcAn.uncertainty::get_parameter_samples(
      pft_names         = pft_name,
      prior_distns_list = list(ma$post.distns),
      trait_mcmc_list   = list(ma$trait.mcmc),
      ensemble.size     = ensemble.size,
      sa_quantiles      = c(0.025, 0.25, 0.5, 0.75, 0.975),
      do_ensemble       = TRUE
    )

    test_that("get_parameter_samples consumes MA output and returns the samples list", {
      expect_named(
        samples,
        c("trait.samples", "sa.samples", "ensemble.samples",
          "runs.samples", "env.samples"),
        ignore.order = TRUE
      )
      expect_type(samples$trait.samples, "list")
    })
  })

  PEcAn.logger::logger.info(
    "Modular chain integration test passed: trait -> meta-analysis -> samples"
  )
}

test_modular_chain(
  pft_name    = "temperate.deciduous",
  modeltype   = "SIPNET",
  trait_names = c("SLA")
)
