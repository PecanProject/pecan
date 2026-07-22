#!/usr/bin/env Rscript
# =============================================================================
# Generate statewide event files for a single output year (Parquet + PEcAn JSON).
#
# Orchestrator only: load matched assignments, dispatch to _lib/build_* modules,
# write outputs under event_files/.
#
#   Phenology: phenology_statewide_{year}.parquet / .json
#   Planting:  planting_statewide_{year}.parquet / .json
#   Harvest:   harvest_statewide_{year}.parquet / .json
#   Tillage:   tillage_statewide_{year}.parquet / .json
#
# USAGE
# -----
#   Rscript make_events_statewide.R <year> [event_type]
#   event_type (optional): phenology | planting | harvest | tillage
#   Default (no event_type): phenology + planting + harvest (not tillage).
#
# Implementation
# --------------
#   _lib/matched_input.R   — read assigned_year=Y, filter matched rows
#   _lib/phenology_events.R — leaf-on/off from MSLSP (format only)
#   _lib/planting_events.R  — C/N pools via traits/pool_calculations_from_lookup.R
#   _lib/harvest_events.R   — removal fractions, young-woody skip, CLASS-level
#                             woody destructive (LandIQ year → year+1 look-ahead)
#   _lib/tillage_events.R   — NDTI + tillage_metrics (separate data path)
#   _lib/io.R               — shared parquet + PEcAn JSON writer
#
# ENV
# ---
#   CCMMF_LANDIQ_V4, HARVEST_LOOKUP_RDS, HARVEST_WOODY_DESTRUCTIVE,
#   TILLAGE_BUFFER_YEARS, TILLAGE_PARCEL_CHUNK — see scripts/events/README.md
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(dplyr)
  library(jsonlite)
  library(lubridate)
})

.fa <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1L])
source(file.path(dirname(normalizePath(.fa, mustWork = FALSE)), "_lib", "bootstrap.R"))
load_events_lib()

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L) {
  stop("Usage: Rscript make_events_statewide.R <year> [phenology|planting|harvest|tillage]")
}
year_arg <- as.integer(args[1L])
if (is.na(year_arg)) {
  stop("Year must be an integer, got: ", args[1L])
}
event_type <- if (length(args) >= 2L) {
  match.arg(args[2L], c("phenology", "planting", "harvest", "tillage"))
} else {
  NULL
}

run_phenology <- is.null(event_type) || event_type == "phenology"
run_planting <- is.null(event_type) || event_type == "planting"
run_harvest <- is.null(event_type) || event_type == "harvest"
run_tillage <- !is.null(event_type) && event_type == "tillage"

msg_suffix <- if (is.null(event_type)) {
  " (phenology + planting + harvest)"
} else {
  paste0(" event_type=", event_type)
}
message("[make_events_statewide] year=", year_arg, msg_suffix)

paths <- events_paths()
dir.create(paths$out_dir, recursive = TRUE, showWarnings = FALSE)

if (run_phenology || run_planting || run_harvest) {
  matched <- load_matched_for_events(
    year_arg,
    paths$matched_dir,
    run_phenology = run_phenology,
    run_planting = run_planting,
    run_harvest = run_harvest
  )
}

if (run_planting || run_harvest) {
  pool <- load_events_trait_pool(paths$pool_script)
}

if (run_phenology) {
  build_phenology_events(matched, year_arg, paths$out_dir)
}

if (run_planting) {
  build_planting_events(matched, year_arg, paths$out_dir, pool$pool_env, pool$lk)
}

if (run_harvest) {
  build_harvest_events(matched, year_arg, paths$out_dir, pool$pool_env, pool$lk)
}

if (run_tillage) {
  build_tillage_events(
    year_arg,
    paths$out_dir,
    paths$matched_dir,
    paths$ndti_root,
    paths$tillage_metrics_script
  )
}

message("[make_events_statewide] Done for year=", year_arg)
