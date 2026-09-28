#!/usr/bin/env Rscript
# Extract MSLSP metrics from tile NetCDF files to per-tile tilepieces (CSV.gz).
#
# USAGE
#   Rscript extract_tiles.R <year> [tile_id] [overwrite]
#
# ENV -- see README.md

.fa <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1L])
.lib <- dirname(normalizePath(.fa, mustWork = FALSE))
source(file.path(.lib, "R", "pkg_root.R"))
load_mslsp_extract()

argv <- commandArgs(trailingOnly = TRUE)
source(file.path(.lib, "R", "mslsp_cli.R"))
year <- parse_cli_year(argv)
overwrite <- parse_cli_overwrite(argv)
tile <- parse_cli_tile(argv)

run_mslsp_extract(year, overwrite = overwrite, tile = tile)
