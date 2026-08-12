#!/usr/bin/env Rscript
# Aggregate tilepieces into ndti_year=Y_month=MM.parquet for one calendar month.
#
# USAGE
#   Rscript combine_month.R <year> <month 1-12> [overwrite]
#
# ENV — see README.md

.fa <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1L])
source(file.path(dirname(normalizePath(.fa, mustWork = FALSE)), "R", "pkg_root.R"))
load_ndti_extract()

argv <- commandArgs(trailingOnly = TRUE)
source(file.path(dirname(normalizePath(.fa, mustWork = FALSE)), "R", "ndti_cli.R"))
year <- parse_cli_year(argv)
month <- parse_cli_month(argv)
overwrite <- parse_cli_overwrite(argv)

run_ndti_combine(year, month, overwrite = overwrite)
