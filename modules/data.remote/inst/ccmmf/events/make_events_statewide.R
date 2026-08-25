#!/usr/bin/env Rscript
# =============================================================================
# Generate statewide event files for a single output year (Parquet + PEcAn JSON).
#
# Orchestrator only: dispatch to R/build_* modules, write parquet + JSON
# under event_files/. Planting, harvest, and tillage copy SIPNET columns
# from apply-script tables. Phenology formats MSLSP dates from the overlay
# as leafon / leafoff rows. Source / diagnostic columns stay on the apply
# tables (recoverable from those parquets).
#
#   Phenology: assigned_year={year}_phenology.parquet / .json
#   Planting:  assigned_year={year}_planting.parquet / .json
#   Harvest:   assigned_year={year}_harvest.parquet / .json
#   Tillage:   assigned_year={year}_tillage.parquet / .json
#
# USAGE
# -----
#   Rscript make_events_statewide.R <prior_year> <target_year> <event_type>
#   Rscript make_events_statewide.R <year> <event_type>
#   event_type (required): phenology | planting | harvest | tillage
#   No default: every type is opt-in. Skip a type by omitting that line.
#
# Implementation
# --------------
#   R/matched_input.R   -- read assigned_year=Y, filter matched rows
#   R/phenology_events.R -- leafon / leafoff from MSLSP (format only)
#   R/planting_events.R  -- SIPNET planting C/N columns from assigned_year=Y_planting
#                             (run traits/apply_planting.R first)
#   R/harvest_events.R   -- SIPNET harvest columns from assigned_year=Y_harvest
#                             (run traits/apply_harvest.R first)
#   R/tillage_events.R   -- SIPNET tillage columns from assigned_year=Y_tillage
#                             (run tillage/apply_tillage.R first)
#   R/io.R               -- shared parquet + PEcAn JSON writer
#
# ENV
# ---
#   MATCHED_DIR -- input overlay (and planting/harvest apply tables)
#   EVENT_OUTPUT_DIR -- event files (default $PRODUCTS_INVENTORY/event_files)
#   Tillage metrics: tillage/apply_tillage.R (HLS_DOWNLOAD_BUFFER_DAYS, TILLAGE_PARCEL_CHUNK)
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(dplyr)
  library(jsonlite)
  library(lubridate)
})

.fa <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1L])
source(file.path(dirname(normalizePath(.fa, mustWork = FALSE)), "R", "bootstrap.R"))
load_events_lib()

.valid_types <- c("phenology", "planting", "harvest", "tillage")
.args_usage <- paste0(
  "Usage: Rscript make_events_statewide.R <prior_year> <target_year> <",
  paste(.valid_types, collapse = "|"),
  ">\n",
  "   or: Rscript make_events_statewide.R <year> <",
  paste(.valid_types, collapse = "|"),
  ">\n",
  "  event_type is required (no default)."
)
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2L || length(args) > 3L) {
  stop(.args_usage)
}
event_type <- tryCatch(
  match.arg(args[length(args)], .valid_types),
  error = function(e) stop(.args_usage, call. = FALSE)
)
year_args <- args[-length(args)]
years <- as.integer(year_args)
if (any(is.na(years))) {
  stop("Years must be integers, got: ", paste(year_args, collapse = " "))
}
years <- unique(years)

run_phenology <- event_type == "phenology"
run_planting <- event_type == "planting"
run_harvest <- event_type == "harvest"
run_tillage <- event_type == "tillage"

paths <- events_paths()
dir.create(paths$out_dir, recursive = TRUE, showWarnings = FALSE)
message(
  "[make_events_statewide] MATCHED_DIR (input)=", paths$matched_dir,
  " EVENT_OUTPUT_DIR=", paths$out_dir
)

for (year_arg in years) {
  message("[make_events_statewide] year=", year_arg, " event_type=", event_type)

  if (run_phenology) {
    matched <- load_matched_for_events(
      year_arg,
      paths$matched_dir,
      run_phenology = TRUE
    )
    build_phenology_events(matched, year_arg, paths$out_dir)
  }

  if (run_planting) {
    build_planting_events(year_arg, paths$out_dir, paths$matched_dir)
  }

  if (run_harvest) {
    build_harvest_events(year_arg, paths$out_dir, paths$matched_dir)
  }

  if (run_tillage) {
    build_tillage_events(
      year_arg,
      paths$out_dir,
      paths$tillage_metrics_dir
    )
  }

  message("[make_events_statewide] Done for year=", year_arg, " event_type=", event_type)
}
