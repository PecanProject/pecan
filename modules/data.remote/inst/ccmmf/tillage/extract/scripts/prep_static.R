#!/usr/bin/env Rscript
# Build/load the per-year NDTI static prep cache (ag parcels, geometry, tile map).
#
# USAGE
#   Rscript prep_static.R <year>
#
# ENV
#   TILLAGE_ROOT, CCMMF_LANDIQ_V4, CCMMF_MANAGEMENT, NDTI_PARCEL_TILEMAP,
#   HLS_IMAGERY_ROOT — see README.md

.fa <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1L])
source(file.path(dirname(normalizePath(.fa, mustWork = FALSE)), "R", "pkg_root.R"))
load_ndti_extract()

argv <- commandArgs(trailingOnly = TRUE)
source(file.path(dirname(normalizePath(.fa, mustWork = FALSE)), "R", "ndti_cli.R"))
year <- parse_cli_year(argv)

run_ndti_prep_static(year)
