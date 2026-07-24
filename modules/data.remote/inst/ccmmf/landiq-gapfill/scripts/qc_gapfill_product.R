#!/usr/bin/env Rscript
# =============================================================================
# QC summary for the gap-filled LandIQ product.
#
# Writes provenance counts (how many rows were gap-filled vs observed) to
# outputs/qc_gapfill_report.md and outputs/qc_gapfill_summary.csv.
#
# ENV
#   CCMMF_LANDIQ_GAPFILL_PRODUCT  — product directory
#   LANDIQ_GAPFILL_RUN_YEARS      — years to summarize (set by run_gapfill.sh)
# =============================================================================

.libPaths(c(file.path(R.home(), "library"), .libPaths()))
suppressPackageStartupMessages({
  library(dplyr)
  library(arrow)
  library(readr)
})

.fa <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1L])
source(file.path(dirname(normalizePath(.fa, mustWork = FALSE)), "R", "pkg_root.R"))
load_landiq_gapfill()
source(file.path(landiq_gapfill_pkg_root(), "scripts", "R", "qc_gapfill_product.R"))

qc_gapfill_product(years = resolve_gapfill_run_years())
