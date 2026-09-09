#!/usr/bin/env Rscript
# Aggregate tilepieces into ndti_year=Y_month=MM.parquet.
# Default: same month set as extract_tiles.R (months 1-12 + forward shoulder).
# Optional month 1-12 for a rerun (no shoulder).
#
# USAGE
#   Rscript combine_year.R <year> [tile_id] [overwrite]
#   Rscript combine_year.R <year> <month 1-12> [overwrite]
#
# ENV -- see README.md
# DEMO_TILE / TILEWISE_ONE_TILE also restrict prep to one tile (same as extract).

.fa <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1L])
.lib <- dirname(normalizePath(.fa, mustWork = FALSE))
source(file.path(.lib, "R", "pkg_root.R"))
load_ndti_extract()

argv <- commandArgs(trailingOnly = TRUE)
source(file.path(.lib, "R", "ndti_cli.R"))
year <- parse_cli_year(argv)
month <- parse_cli_month_optional(argv)
tile <- parse_cli_tile(argv)
overwrite <- parse_cli_overwrite(argv)

run_ndti_combine(year, month = month, overwrite = overwrite, tile = tile)
