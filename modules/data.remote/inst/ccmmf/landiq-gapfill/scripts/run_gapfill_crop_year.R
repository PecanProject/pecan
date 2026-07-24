#!/usr/bin/env Rscript
# =============================================================================
# Crop identity gap-fill (CLASS / SUBCLASS) for one calendar year — pipeline step 1.
#
# USAGE
#   Rscript run_gapfill_crop_year.R <year>
#
# MODES (auto from year)
#   full        — entire year missing from LandIQ (default: 2017); CLASS + SUBCLASS
#   within_year — LandIQ present; fill missing SUBCLASS only (CLASS X / YP exempt)
#
# ENV
#   LANDIQ_GAPFILL_ROOT, CCMMF_LANDIQ_V4, CDL_OUT_DIR
#   COUNTY_TRANSITION_MATRICES_DIR, EXTERNAL_TRANSITION_MATRIX_CSV (full-year mode)
#   See README.md
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
  stop("Usage: Rscript run_gapfill_crop_year.R <YEAR>")
}

run_gapfill(gapfill_year)
