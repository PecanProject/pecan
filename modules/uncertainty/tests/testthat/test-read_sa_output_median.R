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
# Reference: https://github.com/pecanproject/pecan/issues/<issue-number>

test_that("read.sa.output resolves median (q50) via shared manifest fallback", {
  withr::with_tempdir({
    pft   <- "temperate.coniferous"
    trait <- "growth_resp_factor"
    yr    <- 2004

    # ---- Build a manifest that matches what write.sa.configs() actually writes ----
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

    # ---- Stub out the model output directory so read.output() finds a file ----
    run_outdir <- file.path(getwd(), median_run_id)
    dir.create(run_outdir, recursive = TRUE)

    # Create a minimal NetCDF file with an NPP variable for the target year
    nc_path <- file.path(run_outdir, paste0(yr, ".nc"))
    nc_obj <- ncdf4::nc_create(
      nc_path,
      list(ncdf4::ncvar_def("NPP", "kg m-2 s-1", list(), missval = NA_real_))
    )
    ncdf4::ncvar_put(nc_obj, "NPP", 1.23)
    ncdf4::nc_close(nc_obj)

    # ---- Run read.sa.output() ----
    # Should NOT warn "Run ID invalid or missing" for quantile 50 any more.
    expect_no_warning(
      out <- PEcAn.uncertainty::read.sa.output(
        traits     = trait,
        quantiles  = "50",
        pecandir   = getwd(),
        outdir     = getwd(),
        pft.name   = pft,
        start.year = yr,
        end.year   = yr,
        variable   = PEcAn.utils::convert.expr("NPP")
      ),
      regexp = "Run ID invalid or missing"
    )

    # The median cell must not be NA
    expect_false(
      is.na(out[["50", trait]]),
      label = "median (q50) output should not be NA"
    )
  })
})


test_that("read.sa.output still warns when no median fallback row exists", {
  withr::with_tempdir({
    # Manifest with quantile 50 row that has the wrong pft_name (not "NA"),
    # meaning neither exact match nor shared-median fallback can be found.
    manifest <- data.frame(
      type     = "Sensitivity",
      pft_name = "some.other.pft",
      trait    = "NA",
      quantile = "50",
      run_id   = "SA-median--1",
      stringsAsFactors = FALSE
    )
    write.csv(manifest, "runs_manifest.csv", row.names = FALSE)

    expect_warning(
      PEcAn.uncertainty::read.sa.output(
        traits     = "growth_resp_factor",
        quantiles  = "50",
        pecandir   = getwd(),
        outdir     = getwd(),
        pft.name   = "temperate.coniferous",
        start.year = 2004,
        end.year   = 2004,
        variable   = PEcAn.utils::convert.expr("NPP")
      ),
      regexp = "No run found in manifest"
    )
  })
})
