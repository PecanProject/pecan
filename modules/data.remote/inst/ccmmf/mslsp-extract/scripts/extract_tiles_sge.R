#!/usr/bin/env Rscript
# SGE array entry: extract one tile (SGE_TASK_ID -> line in year=Y/sge_tiles.txt).
#
# USAGE
#   qsub -t 1-N -v MSLSP_YEAR=2024 mslsp-extract/sge/run_mslsp_tiles.sge
#
# sge_tiles.txt is written by prep_static.R: tileids.txt ∩ tiles with ag parcels.
# Prep cache must exist for the year (run prep_static.R first).

.fa <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1L])
.lib <- dirname(normalizePath(.fa, mustWork = FALSE))
source(file.path(.lib, "_lib", "pkg_root.R"))
load_mslsp_extract()

argv <- commandArgs(trailingOnly = TRUE)
source(file.path(.lib, "_lib", "mslsp_cli.R"))

year <- suppressWarnings(as.integer(Sys.getenv("MSLSP_YEAR", "")))
if (is.na(year)) {
  year <- if (length(argv)) parse_cli_year(argv) else NA_integer_
}
if (is.na(year)) {
  stop("Set MSLSP_YEAR for qsub -v or pass <year> on the command line")
}

overwrite <- parse_cli_overwrite(argv) ||
  tolower(Sys.getenv("MSLSP_OVERWRITE", "")) %in% MSLSP_OW_TOKENS

run_mslsp_extract_sge_tile(year, overwrite = overwrite)
