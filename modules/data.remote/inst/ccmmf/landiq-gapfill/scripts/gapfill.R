#!/usr/bin/env Rscript
# =============================================================================
# LandIQ gap-fill CLI -- independent commands for run_gapfill.sh.
#
#   cdl-landiq-probs   rebuild CDL x LandIQ probability tables
#   crop <YEARS>       crop identity gap-fill (YYYY or YYYY,YYYY)
#   adoy-ref           rebuild ADOY reference tables
#   adoy <YEARS>       ADOY gap-fill (YYYY or YYYY,YYYY)
#   merge [YEARS]      join crop+ADOY fills into $LANDIQ_GAPFILLED
#   qc [YEARS]         provenance QC summary
#
# COVER is not a gap-fill command. Use scripts/R/cover_crop_landiq.R after merge.
# =============================================================================

.libPaths(c(file.path(R.home(), "library"), .libPaths()))
suppressPackageStartupMessages({
  library(tidyverse)
  library(arrow)
})

.fa <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1L])
source(file.path(dirname(normalizePath(.fa, mustWork = FALSE)), "R", "pkg_root.R"))
load_landiq_gapfill()

gapfill_main(commandArgs(trailingOnly = TRUE))
