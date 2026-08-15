#!/usr/bin/env Rscript
# Build/load the per-year MSLSP static prep cache (ag parcels, tile map, tiles_to_run.txt).
#
# USAGE
#   Rscript prep_static.R <year> [overwrite]
#
# ENV
#   PHENOLOGY_ROOT, LANDIQ_GAPFILLED, PRODUCTS_INVENTORY, HLS_PARCEL_TILEMAP,
#   MSLSP_NETCDF_ROOT -- see ../README.md (phenology component)

.fa <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1L])
.lib <- dirname(normalizePath(.fa, mustWork = FALSE))
source(file.path(.lib, "R", "pkg_root.R"))
load_mslsp_extract()

argv <- commandArgs(trailingOnly = TRUE)
source(file.path(.lib, "R", "mslsp_cli.R"))
year <- parse_cli_year(argv)
overwrite <- parse_cli_overwrite(argv)

run_mslsp_prep_static(year, overwrite = overwrite)
