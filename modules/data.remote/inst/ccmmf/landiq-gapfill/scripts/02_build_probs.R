#!/usr/bin/env Rscript
# Build CDL emission probability tables. Usually called via ensure_emission_tables().

.libPaths(c(file.path(R.home(), "library"), .libPaths()))
suppressPackageStartupMessages({
  library(tidyverse)
  library(arrow)
})

.fa <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1L])
source(file.path(dirname(normalizePath(.fa, mustWork = FALSE)), "_lib", "pkg_root.R"))
load_landiq_gapfill()

build_emission_prob_tables()
