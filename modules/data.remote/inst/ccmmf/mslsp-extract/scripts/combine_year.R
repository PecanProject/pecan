#!/usr/bin/env Rscript
# Aggregate tilepieces into mslsp_year=Y.parquet for one calendar year.
#
# USAGE
#   Rscript combine_year.R <year> [overwrite]
#
# ENV — see README.md

.fa <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1L])
source(file.path(dirname(normalizePath(.fa, mustWork = FALSE)), "_lib", "pkg_root.R"))
load_mslsp_extract()

argv <- commandArgs(trailingOnly = TRUE)
source(file.path(dirname(normalizePath(.fa, mustWork = FALSE)), "_lib", "mslsp_cli.R"))
year <- parse_cli_year(argv)
overwrite <- parse_cli_overwrite(argv)

run_mslsp_combine(year, overwrite = overwrite)
