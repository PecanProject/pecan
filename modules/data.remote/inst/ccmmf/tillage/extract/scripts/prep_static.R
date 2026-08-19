#!/usr/bin/env Rscript
# Load NDTI prep from parcel_tiles.csv filtered to year ag parcels. Does not
# rebuild the parcel-tile map.
#
# USAGE
#   Rscript prep_static.R <year>
#
# Prerequisite:
#   Rscript $CCMMF_CODE/hls/build_hls_parcel_tile_map.R
#
# ENV
#   TILLAGE_ROOT, PRODUCTS_INVENTORY, HLS_IMAGERY_ROOT, HLS_PARCEL_TILES_DIR
#   -- see ../README.md

.fa <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1L])
.lib <- dirname(normalizePath(.fa, mustWork = FALSE))
source(file.path(.lib, "R", "pkg_root.R"))
load_ndti_extract()

argv <- commandArgs(trailingOnly = TRUE)
source(file.path(.lib, "R", "ndti_cli.R"))
year <- parse_cli_year(argv)

run_ndti_prep_static(year)
