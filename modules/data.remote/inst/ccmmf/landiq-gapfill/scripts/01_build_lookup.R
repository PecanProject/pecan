#!/usr/bin/env Rscript
# Build CDL x LandIQ emission lookup tables. Usually called via ensure_emission_tables().

.libPaths(c(file.path(R.home(), "library"), .libPaths()))
suppressPackageStartupMessages({
  library(tidyverse)
  library(arrow)
})

.fa <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1L])
source(file.path(dirname(normalizePath(.fa, mustWork = FALSE)), "R", "pkg_root.R"))
load_landiq_gapfill()

build_emission_lookup()
