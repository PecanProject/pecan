# Regression test for the shared-median manifest fallback in read.sa.output()
#
# Background: write.sa.configs() writes the median (q50) run as a single shared
# entry with pft_name = "NA" and trait = "NA".  read.sa.output() previously did
# a strict per-trait/per-PFT lookup, so the median row always failed to match
# and the 50th-quantile cell was returned as NA.
#
# The fix adds a fallback: when quantile == "50" and no exact match is found,
# look for the shared median entry before giving up.
#
# Reference: https://github.com/pecanproject/pecan/issues/3882

setup_logger_for_testing <- function() {
  PEcAn.logger::logger.setUseConsole(TRUE, FALSE)
  PEcAn.logger::logger.setLevel("WARN")
}

test_that("read.sa.output resolves median (q50) via shared manifest fallback", {
  skip_if_not_installed("ncdf4")

  withr::with_tempdir({
    setup_logger_for_testing()
    on.exit(PEcAn.logger::logger.setUseConsole(TRUE, TRUE), add = TRUE)

    pft   <- "temperate.coniferous"
    trait <- "growth_resp_factor"
    yr    <- 2004

    # One shared median row (pft_name = "NA", trait = "NA") + no trait-specific row
    # for quantile 50.  This is the exact situation that triggered the original bug.
    median_run_id <- "SA-median--1"
    manifest <- data.frame(
      type     = "Sensitivity",
      pft_name = "NA",
      trait    = "NA",
      quantile = "50",
      run_id   = median_run_id,
      stringsAsFactors = FALSE
    )
    write.csv(manifest, "runs_manifest.csv", row.names = FALSE)

    run_outdir <- file.path(getwd(), median_run_id)
    dir.create(run_outdir, recursive = TRUE)

    nc_path <- file.path(run_outdir, paste0(yr, ".nc"))
    nc_obj <- ncdf4::nc_create(
      nc_path,
      list(ncdf4::ncvar_def("NPP", "kg m-2 s-1", list(), missval = NA_real_))
    )
    ncdf4::ncvar_put(nc_obj, "NPP", 1.23)
    ncdf4::nc_close(nc_obj)

    out <- PEcAn.uncertainty::read.sa.output(
      traits     = trait,
      quantiles  = "50",
      pecandir   = getwd(),
      outdir     = getwd(),
      pft.name   = pft,
      start.year = yr,
      end.year   = yr,
      variable   = PEcAn.utils::convert.expr("NPP")$variable.eqn
    )

    expect_false(
      is.na(out[["50", trait]]),
      label = "median (q50) output should not be NA"
    )
  })
})


test_that("read.sa.output still warns when no median fallback row exists", {
  withr::with_tempdir({
    setup_logger_for_testing()
    on.exit(PEcAn.logger::logger.setUseConsole(TRUE, TRUE), add = TRUE)

    # q50 row with concrete pft_name and trait: no exact match and no NA/NA fallback
    manifest <- data.frame(
      type     = "Sensitivity",
      pft_name = "temperate.coniferous",
      trait    = "other_trait",
      quantile = "50",
      run_id   = "SA-other--1",
      stringsAsFactors = FALSE
    )
    write.csv(manifest, "runs_manifest.csv", row.names = FALSE)

    out <- expect_output(
      PEcAn.uncertainty::read.sa.output(
        traits     = "growth_resp_factor",
        quantiles  = "50",
        pecandir   = getwd(),
        outdir     = getwd(),
        pft.name   = "temperate.coniferous",
        start.year = 2004,
        end.year   = 2004,
        variable   = PEcAn.utils::convert.expr("NPP")$variable.eqn
      ),
      regexp = "No run found in manifest"
    )

    expect_true(is.na(out[["50", "growth_resp_factor"]]))
  })
})
