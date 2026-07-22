# Paths for statewide event generation (make_events_statewide.R).

events_pkg_root <- function() {
  env <- trimws(Sys.getenv("EVENTS_ROOT", ""))
  if (nzchar(env)) {
    return(normalizePath(env, mustWork = FALSE))
  }
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) == 0L) {
    stop("Set EVENTS_ROOT or run via Rscript from scripts/events/.")
  }
  dir <- dirname(normalizePath(sub("^--file=", "", file_arg[1L]), mustWork = FALSE))
  for (k in seq_len(6L)) {
    if (file.exists(file.path(dir, "_lib", "bootstrap.R"))) {
      return(normalizePath(dir, mustWork = FALSE))
    }
    parent <- dirname(dir)
    if (identical(parent, dir)) {
      break
    }
    dir <- parent
  }
  stop("Could not locate scripts/events root. Set EVENTS_ROOT.")
}

events_paths <- function() {
  path_management <- Sys.getenv("CCMMF_MANAGEMENT", "/projectnb/dietzelab/ccmmf/management")
  path_landiq <- Sys.getenv(
    "CCMMF_LANDIQ_V4",
    "/projectnb/dietzelab/ccmmf/LandIQ-harmonized-v4.1.2"
  )
  list(
    management = path_management,
    landiq_dir = path_landiq,
    landiq_crops = file.path(path_landiq, "crops_all_years.parq"),
    cropcode_csv = file.path(path_management, "LandIQ_cropCode_lookup_table.csv"),
    matched_dir = Sys.getenv(
      "CCMMF_MATCHED_DIR",
      file.path(path_management, "phenology", "matched_landiq_mslsp_v4.1.2")
    ),
    pool_script = file.path(path_management, "scripts", "traits", "pool_calculations_from_lookup.R"),
    tillage_metrics_script = file.path(path_management, "scripts", "tillage", "tillage_metrics.R"),
    ndti_root = file.path(path_management, "tillage", "ndti_v4.1"),
    out_dir = file.path(path_management, "event_files")
  )
}

event_output_paths <- function(out_dir, kind, year) {
  list(
    parquet = file.path(out_dir, sprintf("%s_statewide_%d.parquet", kind, year)),
    json = file.path(out_dir, sprintf("%s_statewide_%d.json", kind, year))
  )
}
