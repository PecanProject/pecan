#!/usr/bin/env Rscript
# Phase 0 audit: assigned LandIQ-MSLSP vs gapfill needs (GAPFILL_PLAN.md).
# Summarizes assigned parquet by year; optional LandIQ vs MSLSP parcel counts.
#
# Usage:
#   module load R/4.4.3
#   Rscript gapfill_phase0_audit.R
#   RUN_LANDIQ_MSLSP_OVERLAP_ONLY=1 Rscript gapfill_phase0_audit.R   # overlap CSV only (no assigned re-read)
# Env:
#   PRODUCTS_INVENTORY or CCMMF_ROOT -- source documentation/setup_env.sh
#   MATCHED_DIR -- default phenology/matched_landiq_mslsp_v4.1.2
#   AUDIT_YEAR_MIN, AUDIT_YEAR_MAX -- default 2016 and 2023
#   RUN_LANDIQ_MSLSP_OVERLAP -- if "1", also compare parcel sets (slower; run after main audit)
#   RUN_LANDIQ_MSLSP_OVERLAP_ONLY -- if "1", only write landiq_vs_mslsp_parcel_counts.csv (same year env)

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(dplyr)
})

path_inventory <- Sys.getenv("PRODUCTS_INVENTORY", "")
if (!nzchar(trimws(path_inventory))) {
  .root <- trimws(Sys.getenv("CCMMF_ROOT", ""))
  if (!nzchar(.root)) {
    stop("Set PRODUCTS_INVENTORY or CCMMF_ROOT (source documentation/setup_env.sh).")
  }
  path_inventory <- file.path(.root, "products", "inventory")
}
.path_code <- trimws(Sys.getenv("CCMMF_CODE", ""))
.script_dir <- tryCatch(
  dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1L], mustWork = FALSE)),
  error = function(e) getwd()
)
.matched_candidates <- c(
  if (nzchar(.path_code)) file.path(.path_code, "phenology", "match", "matched_paths.R") else character(),
  file.path(.script_dir, "matched_paths.R"),
  file.path(.script_dir, "..", "match", "matched_paths.R"),
  file.path(path_inventory, "scripts", "phenology", "matched_paths.R")
)
.matched_paths <- .matched_candidates[file.exists(.matched_candidates)][1L]
if (is.na(.matched_paths) || !nzchar(.matched_paths)) {
  stop("Could not find matched_paths.R (set CCMMF_CODE or place next to this script).")
}
source(.matched_paths)
matched_dir <- matched_landiq_dir(path_inventory)
mslsp_root <- file.path(path_inventory, "phenology", "raw_mslsp_v4.1.2")
landiq_parq <- {
  .liq <- trimws(Sys.getenv("LANDIQ_GAPFILLED", ""))
  if (!nzchar(.liq)) {
    .root <- trimws(Sys.getenv("CCMMF_ROOT", ""))
    if (!nzchar(.root)) {
      stop("Set LANDIQ_GAPFILLED or CCMMF_ROOT (source documentation/setup_env.sh).")
    }
    .liq <- file.path(.root, "LandIQ", "gapfilled")
  }
  file.path(.liq, "crops_all_years.parq")
}
out_dir <- file.path(matched_dir, "gapfill_phase0_audit")
year_min <- as.integer(Sys.getenv("AUDIT_YEAR_MIN", "2016"))
year_max <- as.integer(Sys.getenv("AUDIT_YEAR_MAX", "2024"))
run_overlap <- tolower(Sys.getenv("RUN_LANDIQ_MSLSP_OVERLAP", "0")) %in% c("1", "true", "yes")
overlap_only <- tolower(Sys.getenv("RUN_LANDIQ_MSLSP_OVERLAP_ONLY", "0")) %in% c("1", "true", "yes")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

if (overlap_only) {
  if (!file.exists(landiq_parq)) stop("LandIQ parquet not found: ", landiq_parq)
  lookup <- fread(file.path(path_inventory, "LandIQ_cropCode_lookup_table.csv"))
  ag_classes <- unique(lookup[is_agricultural == TRUE, trimws(as.character(CLASS))])
  overlap_dt <- NULL
  for (yr in year_min:year_max) {
    mpath <- file.path(mslsp_root, sprintf("year=%d", yr), sprintf("mslsp_year=%d.parquet", yr))
    if (!file.exists(mpath)) {
      message("Skip year ", yr, " (no MSLSP combined file)")
      next
    }
    ds_li <- open_dataset(landiq_parq) |>
      dplyr::filter(year == !!yr, CLASS %in% !!ag_classes) |>
      dplyr::distinct(parcel_id) |>
      dplyr::collect()
    n_li <- nrow(ds_li)
    m <- as.data.table(read_parquet(mpath))[, .(parcel_id = unique(as.character(parcel_id)))]
    n_m <- nrow(m)
    li_set <- unique(ds_li$parcel_id)
    m_set <- m$parcel_id
    in_both <- length(intersect(li_set, m_set))
    only_li <- length(setdiff(li_set, m_set))
    only_m <- length(setdiff(m_set, li_set))
    overlap_dt <- rbind(overlap_dt, data.table(
      year = yr, n_landiq_ag_parcels = n_li, n_mslsp_parcels = n_m,
      n_both = in_both, n_landiq_only = only_li, n_mslsp_only = only_m
    ))
  }
  if (is.null(overlap_dt)) stop("No overlap rows written (check years and MSLSP paths)")
  fwrite(overlap_dt, file.path(out_dir, "landiq_vs_mslsp_parcel_counts.csv"))
  message("Wrote ", file.path(out_dir, "landiq_vs_mslsp_parcel_counts.csv"))
  quit(save = "no")
}

# Harvest date used in make_events_statewide.R: row/rice -> OGMn; hay/woody -> OGD
has_harvest_event_date <- function(DT) {
  p <- tolower(trimws(as.character(DT$landiq_PFT)))
  ogmn_ok <- !is.na(DT$mslsp_OGMn)
  ogd_ok <- !is.na(DT$mslsp_OGD)
  fifelse(
    p %in% c("row", "rice"), ogmn_ok,
    fifelse(p %in% c("hay", "woody"), ogd_ok, FALSE)
  )
}

audit_one_year <- function(yr) {
  path <- file.path(matched_dir, sprintf("assigned_year=%d.parquet", yr))
  if (!file.exists(path)) {
    message("Missing: ", path)
    return(list(rows = NULL, parcel_year = NULL, summaries = NULL))
  }
  d <- as.data.table(read_parquet(path))
  d[, parcel_id := as.character(parcel_id)]
  d[, year := as.integer(year)]

  # --- Row-level flags (one row per parcel-year-season) ---
  d[, `:=`(
    has_landiq_crop = !is.na(landiq_CLASS) & !is.na(landiq_SUBCLASS) & !is.na(landiq_PFT),
    has_adoy = !is.na(landiq_ADOY),
    has_mslsp_planting = !is.na(mslsp_OGI),
    has_mslsp_ogmn = !is.na(mslsp_OGMn),
    has_mslsp_ogd = !is.na(mslsp_OGD)
  )]
  d[, has_mslsp_harvest_metric := has_harvest_event_date(d)]

  # Parcel-year grain: any season row with crop / matched / etc.
  py <- d[, .(
    n_season_rows = .N,
    has_landiq_crop = any(has_landiq_crop),
    has_adoy_any = any(has_adoy & has_landiq_crop),
    has_no_mslsp = any(assigned_by == "no_mslsp"),
    has_mslsp = !any(assigned_by == "no_mslsp"),
    has_matched = any(assigned_by == "matched"),
    has_matched_with_crop = any(assigned_by == "matched" & has_landiq_crop),
    has_mslsp_planting_matched = any(assigned_by == "matched" & has_mslsp_planting),
    has_harvest_metric_matched = any(assigned_by == "matched" & has_mslsp_harvest_metric)
  ), by = .(parcel_id, year)]

  summ_assigned <- d[, .N, by = assigned_by][, year := yr]
  summ_match <- d[, .N, by = match_outcome][, year := yr]

  # QC dimensions (optional; full dump can be large)
  qc_cols <- grep("^qc_", names(d), value = TRUE)
  qc_key <- intersect(
    qc_cols,
    c(
      "qc_adoy_vs_cycle", "qc_n_adoy_in_cycle", "qc_cycle_season_counts",
      "qc_mslsp_pixel_availability", "qc_heterogeneity", "qc_mslsp_qa_pixel_agreement"
    )
  )
  qc_use <- if (length(qc_key)) qc_key else qc_cols
  qc_long <- NULL
  for (cc in qc_use) {
    tab <- d[, .N, by = c(cc)]
    setnames(tab, cc, "category")
    tab[, qc_dimension := cc]
    qc_long <- rbind(qc_long, tab[, .(year = yr, qc_dimension, category, N)])
  }

  gap_rows <- d[has_landiq_crop == TRUE, .(
    n = .N,
    pct_adoy = 100 * mean(has_adoy),
    pct_mslsp_OGI = 100 * mean(has_mslsp_planting),
    pct_mslsp_OGMn = 100 * mean(has_mslsp_ogmn),
    pct_mslsp_OGD = 100 * mean(has_mslsp_ogd),
    pct_harvest_metric = 100 * mean(has_mslsp_harvest_metric)
  )]
  gap_rows[, year := yr]

  list(
    path = path,
    n_rows = nrow(d),
    n_parcel_year = nrow(py),
    summaries = list(assigned_by = summ_assigned, match_outcome = summ_match, qc_long = qc_long, gap_landiq_crop_rows = gap_rows),
    parcel_year = py
  )
}

years <- year_min:year_max
paths_exist <- vapply(years, function(yr) {
  file.exists(file.path(matched_dir, sprintf("assigned_year=%d.parquet", yr)))
}, logical(1))
years <- years[paths_exist]
if (length(years) == 0L) {
  stop("No assigned_year=*.parquet found under ", matched_dir, " for AUDIT_YEAR_MIN:AUDIT_YEAR_MAX")
}
message("Auditing years: ", paste(years, collapse = ", "))
missing_years <- setdiff(year_min:year_max, years)
if (length(missing_years)) {
  message("Note: no assigned file for year(s): ", paste(missing_years, collapse = ", "))
}
results <- lapply(years, audit_one_year)
names(results) <- as.character(years)

# Combined tables
assigned_all <- rbindlist(lapply(results, function(x) x$summaries$assigned_by), fill = TRUE)
match_all <- rbindlist(lapply(results, function(x) x$summaries$match_outcome), fill = TRUE)
gap_all <- rbindlist(lapply(results, function(x) x$summaries$gap_landiq_crop_rows), fill = TRUE)
qc_all <- rbindlist(lapply(results, function(x) x$summaries$qc_long), fill = TRUE)

fwrite(assigned_all[order(year, assigned_by)], file.path(out_dir, "counts_by_assigned_by.csv"))
fwrite(match_all[order(year, match_outcome)], file.path(out_dir, "counts_by_match_outcome.csv"))
fwrite(gap_all, file.path(out_dir, "landiq_crop_rows_pct_nonmissing_mslsp_adoy.csv"))
fwrite(qc_all, file.path(out_dir, "counts_by_qc_column.csv"))

# Parcel-year rollup table
py_list <- lapply(names(results), function(yn) {
  x <- results[[yn]]$parcel_year
  if (is.null(x) || nrow(x) == 0) return(NULL)
  x
})
py_all <- rbindlist(py_list, fill = TRUE)
if (nrow(py_all) > 0) {
  py_sum <- py_all[, .(
    n_py = .N,
    n_has_landiq_crop = sum(has_landiq_crop),
    n_no_mslsp = sum(has_no_mslsp),
    n_with_mslsp = sum(has_mslsp),
    n_has_matched = sum(has_matched),
    n_matched_with_crop = sum(has_matched_with_crop),
    n_matched_crop_no_planting = sum(has_matched_with_crop & !has_mslsp_planting_matched),
    n_matched_crop_no_harvest_metric = sum(has_matched_with_crop & !has_harvest_metric_matched),
    pct_py_adoy_among_crop = 100 * mean(has_adoy_any[has_landiq_crop == TRUE]),
    pct_no_mslsp_among_crop = 100 * mean(has_no_mslsp[has_landiq_crop == TRUE])
  ), by = year]
  fwrite(py_sum, file.path(out_dir, "parcel_year_gap_summary.csv"))
  if (tolower(Sys.getenv("SAVE_PARCEL_YEAR_LONG", "0")) %in% c("1", "true", "yes")) {
    fwrite(py_all, file.path(out_dir, "parcel_year_flags_long.csv"))
  }
}

# --- Optional: LandIQ agricultural parcels vs MSLSP combined parcels ---
overlap_dt <- NULL
if (run_overlap && file.exists(landiq_parq)) {
  lookup <- fread(file.path(path_inventory, "LandIQ_cropCode_lookup_table.csv"))
  ag_classes <- unique(lookup[is_agricultural == TRUE, trimws(as.character(CLASS))])
  for (yr in years) {
    mpath <- file.path(mslsp_root, sprintf("year=%d", yr), sprintf("mslsp_year=%d.parquet", yr))
    if (!file.exists(mpath)) next
    ds_li <- open_dataset(landiq_parq) |>
      dplyr::filter(year == !!yr, CLASS %in% !!ag_classes) |>
      dplyr::distinct(parcel_id) |>
      dplyr::collect()
    n_li <- nrow(ds_li)
    m <- as.data.table(read_parquet(mpath))[, .(parcel_id = unique(as.character(parcel_id)))]
    n_m <- nrow(m)
    li_set <- unique(ds_li$parcel_id)
    m_set <- m$parcel_id
    in_both <- length(intersect(li_set, m_set))
    only_li <- length(setdiff(li_set, m_set))
    only_m <- length(setdiff(m_set, li_set))
    overlap_dt <- rbind(overlap_dt, data.table(
      year = yr, n_landiq_ag_parcels = n_li, n_mslsp_parcels = n_m,
      n_both = in_both, n_landiq_only = only_li, n_mslsp_only = only_m
    ))
  }
  if (!is.null(overlap_dt)) fwrite(overlap_dt, file.path(out_dir, "landiq_vs_mslsp_parcel_counts.csv"))
}

message("Wrote Phase 0 audit tables to: ", out_dir)
