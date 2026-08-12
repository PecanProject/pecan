#!/usr/bin/env Rscript
# Optional array entry: extract one tile (TASK_ID -> line in year=Y/tiles_to_run.txt).
#
# Portable one-tile runs prefer:
#   ./run_mslsp.sh --tile TILEID --no-combine YEAR
#
# Array adapters (any scheduler that sets TASK_ID):
#   export MSLSP_YEAR=2024 TASK_ID=1
#   Rscript scripts/extract_tiles_task.R
#   # or: ./run_mslsp.sh --task-tile 2024
#
# tiles_to_run.txt is written by prep_static.R: tileids.txt intersect tiles with ag parcels.
# Prep cache must exist for the year (run prep_static.R / --prep-only first).

.fa <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1L])
.lib <- dirname(normalizePath(.fa, mustWork = FALSE))
source(file.path(.lib, "R", "pkg_root.R"))
load_mslsp_extract()

argv <- commandArgs(trailingOnly = TRUE)
source(file.path(.lib, "R", "mslsp_cli.R"))

year <- suppressWarnings(as.integer(Sys.getenv("MSLSP_YEAR", "")))
if (is.na(year)) {
  year <- if (length(argv)) parse_cli_year(argv) else NA_integer_
}
if (is.na(year)) {
  stop("Set MSLSP_YEAR or pass <year> on the command line")
}

overwrite <- parse_cli_overwrite(argv) ||
  tolower(Sys.getenv("MSLSP_OVERWRITE", "")) %in% MSLSP_OW_TOKENS

run_mslsp_extract_task_tile(year, overwrite = overwrite)
