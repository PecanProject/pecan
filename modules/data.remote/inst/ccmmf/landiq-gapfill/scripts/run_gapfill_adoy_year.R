#!/usr/bin/env Rscript
# =============================================================================
# ADOY gap-fill for one calendar year -- pipeline step 2 (after run_gapfill_crop_year.R).
#
# USAGE
#   Rscript run_gapfill_adoy_year.R <year>
#
# Never overwrites existing valid ADOY. Exempt CLASS: X, I (not YP).
# Fill tiers: county CLASS+SUBCLASS, temporal neighbor, county CLASS,
#             statewide CLASS+SUBCLASS, statewide CLASS.
#
# ENV
#   LANDIQ_GAPFILL_ROOT, CCMMF_LANDIQ_V4
#   GAPFILL_REBUILD_ADOY_REF=1  -- force reference-table rebuild
# =============================================================================

.libPaths(c(file.path(R.home(), "library"), .libPaths()))
suppressPackageStartupMessages({
  library(tidyverse)
  library(arrow)
})

.fa <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1L])
source(file.path(dirname(normalizePath(.fa, mustWork = FALSE)), "R", "pkg_root.R"))
load_landiq_gapfill()

gapfill_year <- suppressWarnings(as.integer(commandArgs(trailingOnly = TRUE)[1L]))
if (is.na(gapfill_year)) {
  gapfill_year <- suppressWarnings(as.integer(Sys.getenv("GAPFILL_YEAR", "")))
}
if (is.na(gapfill_year)) {
  stop("Usage: Rscript run_gapfill_adoy_year.R <YEAR>")
}

ensure_adoy_reference()
run_adoy_gapfill(gapfill_year)
