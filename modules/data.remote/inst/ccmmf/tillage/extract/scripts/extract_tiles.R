#!/usr/bin/env Rscript
# Extract NDTI from HLS reflectance per tile -> tilepieces (CSV.gz).
# Default: months overlapping Jan 1 Y through Dec 31 Y plus
# HLS_DOWNLOAD_BUFFER_DAYS (185; hive year=Y+1). Optional
# month 1-12 for a rerun (no shoulder).
#
# USAGE
#   Rscript extract_tiles.R <year> [tile_id] [overwrite]
#   Rscript extract_tiles.R <year> <month 1-12> [overwrite]
#
# ENV -- see README.md

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

run_ndti_extract(year, month = month, overwrite = overwrite, tile = tile)
