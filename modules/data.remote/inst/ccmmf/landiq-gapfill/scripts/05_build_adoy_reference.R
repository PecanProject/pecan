#!/usr/bin/env Rscript
# =============================================================================
# Rebuild ADOY reference tables only (no per-year fill).
# Normally run_gapfill_adoy_year.R calls ensure_adoy_reference() first.
#
# USAGE
#   Rscript 05_build_adoy_reference.R
# =============================================================================

.libPaths(c(file.path(R.home(), "library"), .libPaths()))
suppressPackageStartupMessages({
  library(tidyverse)
  library(arrow)
})

.fa <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1L])
source(file.path(dirname(normalizePath(.fa, mustWork = FALSE)), "R", "pkg_root.R"))
load_landiq_gapfill()

build_adoy_reference()
