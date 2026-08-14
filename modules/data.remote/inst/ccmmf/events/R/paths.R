# Paths for statewide event generation (make_events_statewide.R).
# Prefer env vars; else derive from CCMMF_ROOT. Source documentation/setup_env.sh.

events_pkg_root <- function() {
  env <- trimws(Sys.getenv("EVENTS_ROOT", ""))
  if (nzchar(env)) {
    return(normalizePath(env, mustWork = FALSE))
  }
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) == 0L) {
    stop("Set EVENTS_ROOT or run via Rscript from the events/ component.")
  }
  dir <- dirname(normalizePath(sub("^--file=", "", file_arg[1L]), mustWork = FALSE))
  for (k in seq_len(6L)) {
    if (file.exists(file.path(dir, "R", "bootstrap.R"))) {
      return(normalizePath(dir, mustWork = FALSE))
    }
    parent <- dirname(dir)
    if (identical(parent, dir)) {
      break
    }
    dir <- parent
  }
  stop("Could not locate events/ root. Set EVENTS_ROOT.")
}

events_paths <- function() {
  path_inventory <- trimws(Sys.getenv("PRODUCTS_INVENTORY", ""))
  if (!nzchar(path_inventory)) {
    root <- trimws(Sys.getenv("CCMMF_ROOT", ""))
    if (!nzchar(root)) {
      stop("Set PRODUCTS_INVENTORY or CCMMF_ROOT (source documentation/setup_env.sh).")
    }
    path_inventory <- file.path(root, "products", "inventory")
  }
  path_landiq <- trimws(Sys.getenv("LANDIQ_GAPFILLED", ""))
  if (!nzchar(path_landiq)) {
    root <- trimws(Sys.getenv("CCMMF_ROOT", ""))
    if (!nzchar(root)) {
      stop("Set LANDIQ_GAPFILLED or CCMMF_ROOT (source documentation/setup_env.sh).")
    }
    path_landiq <- file.path(root, "LandIQ", "gapfilled")
  }
  path_code <- trimws(Sys.getenv("CCMMF_CODE", ""))
  traits_root <- trimws(Sys.getenv("TRAITS_ROOT", ""))
  if (!nzchar(traits_root) && nzchar(path_code)) {
    traits_root <- file.path(path_code, "traits")
  }
  if (!nzchar(traits_root)) {
    traits_root <- file.path(path_inventory, "scripts", "traits")
  }
  events_root <- events_pkg_root()
  tillage_metrics_script <- file.path(events_root, "R", "tillage_metrics.R")
  if (!file.exists(tillage_metrics_script)) {
    tillage_metrics_script <- file.path(path_inventory, "scripts", "tillage", "tillage_metrics.R")
  }
  list(
    inventory = path_inventory,
    landiq_dir = path_landiq,
    landiq_crops = file.path(path_landiq, "crops_all_years.parq"),
    cropcode_csv = file.path(path_inventory, "LandIQ_cropCode_lookup_table.csv"),
    matched_dir = {
      md <- trimws(Sys.getenv("MATCHED_DIR", ""))
      if (nzchar(md)) md else file.path(path_inventory, "phenology", "matched_landiq_mslsp_v4.1.2")
    },
    pool_script = file.path(traits_root, "pool_calculations_from_lookup.R"),
    tillage_metrics_script = tillage_metrics_script,
    ndti_root = file.path(path_inventory, "tillage", "ndti_v4.1.2"),
    out_dir = {
      od <- trimws(Sys.getenv("EVENT_OUTPUT_DIR", ""))
      if (nzchar(od)) od else file.path(path_inventory, "event_files_v4.1.2")
    }
  )
}

event_output_paths <- function(out_dir, kind, year) {
  list(
    parquet = file.path(out_dir, sprintf("%s_statewide_%d.parquet", kind, year)),
    json = file.path(out_dir, sprintf("%s_statewide_%d.json", kind, year))
  )
}

# Prior-year fallows found while running job_year (lookback). Safe for parallel
# year jobs; merge_tillage_lookback() folds these into the canonical files.
tillage_lookback_amend_path <- function(out_dir, prior_year, job_year) {
  file.path(
    out_dir,
    sprintf("tillage_statewide_%d_lookback_from_%d.parquet", prior_year, job_year)
  )
}
