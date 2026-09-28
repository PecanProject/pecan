#!/usr/bin/env Rscript
# Load MSLSP prep from parcel_tiles.csv filtered to year ag parcels; optionally
# write tiles_to_run.txt for array extract (TASK_ID). Does not rebuild the map.
#
# USAGE
#   Rscript prep_static.R <year> [overwrite]
#
# Prerequisite:
#   Rscript $CCMMF_CODE/hls/build_hls_parcel_tile_map.R
#
# ENV
#   PHENOLOGY_ROOT, PRODUCTS_INVENTORY, MSLSP_NETCDF_ROOT, HLS_PARCEL_TILES_DIR
#   -- see ../README.md

.fa <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1L])
.lib <- dirname(normalizePath(.fa, mustWork = FALSE))
source(file.path(.lib, "R", "pkg_root.R"))
load_mslsp_extract()

argv <- commandArgs(trailingOnly = TRUE)
source(file.path(.lib, "R", "mslsp_cli.R"))
year <- parse_cli_year(argv)
overwrite <- parse_cli_overwrite(argv)

run_mslsp_prep_static(year, overwrite = overwrite)
