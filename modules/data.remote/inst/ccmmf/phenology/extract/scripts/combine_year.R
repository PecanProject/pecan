#!/usr/bin/env Rscript
# Aggregate tilepieces into mslsp_year=Y.parquet for one calendar year.
#
# USAGE
#   Rscript combine_year.R <year> [tile_id] [overwrite]
#
# ENV -- see README.md
# DEMO_TILE / TILEWISE_ONE_TILE also restrict prep to one tile (same as extract).

.fa <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1L])
source(file.path(dirname(normalizePath(.fa, mustWork = FALSE)), "R", "pkg_root.R"))
load_mslsp_extract()

argv <- commandArgs(trailingOnly = TRUE)
source(file.path(dirname(normalizePath(.fa, mustWork = FALSE)), "R", "mslsp_cli.R"))
year <- parse_cli_year(argv)
overwrite <- parse_cli_overwrite(argv)
tile <- parse_cli_tile(argv)

run_mslsp_combine(year, overwrite = overwrite, tile = tile)
