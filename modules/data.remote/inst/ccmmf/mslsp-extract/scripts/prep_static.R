#!/usr/bin/env Rscript
# Build/load the per-year MSLSP static prep cache (ag parcels, tile map, sge_tiles.txt).
#
# USAGE
#   Rscript prep_static.R <year> [overwrite]
#
# ENV
#   MSLSP_EXTRACT_ROOT, CCMMF_LANDIQ_V4, CCMMF_MANAGEMENT, mslsp_parcel_tilemap,
#   mslsp_legacy_dir, mslsp_new_base — see README.md

.fa <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1L])
.lib <- dirname(normalizePath(.fa, mustWork = FALSE))
source(file.path(.lib, "_lib", "pkg_root.R"))
load_mslsp_extract()

argv <- commandArgs(trailingOnly = TRUE)
source(file.path(.lib, "_lib", "mslsp_cli.R"))
year <- parse_cli_year(argv)
overwrite <- parse_cli_overwrite(argv)

run_mslsp_prep_static(year, overwrite = overwrite)
