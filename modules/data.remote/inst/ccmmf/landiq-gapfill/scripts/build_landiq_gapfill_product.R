#!/usr/bin/env Rscript
# =============================================================================
# Build gap-filled LandIQ harmonized product — pipeline step 3.
#
# Merges source LandIQ with per-year crop and ADOY gap-fill outputs.
# Writes crops_all_years.parq + parcels-consolidated.gpkg symlink.
#
# ENV
#   CCMMF_LANDIQ_GAPFILL_PRODUCT  — output directory
#   LANDIQ_GAPFILL_START_YEAR / END_YEAR (or RUN_YEARS) — years to patch
# =============================================================================

.libPaths(c(file.path(R.home(), "library"), .libPaths()))
suppressPackageStartupMessages({
  library(tidyverse)
  library(arrow)
})

.fa <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1L])
source(file.path(dirname(normalizePath(.fa, mustWork = FALSE)), "_lib", "pkg_root.R"))
load_landiq_gapfill()

build_landiq_product(years = resolve_gapfill_run_years())
