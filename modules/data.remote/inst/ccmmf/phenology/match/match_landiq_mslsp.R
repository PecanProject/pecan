#!/usr/bin/env Rscript
# =====================================================================
# match_landiq_mslsp.R
# Rule-based matching (no rank-based cost).
#
# Usage:
#   Rscript match_landiq_mslsp.R YEAR
#   Rscript -e "YEAR <- 2024; source('match_landiq_mslsp.R')"
#
# LandIQ inventory: all ag parcel-years from LANDIQ_GAPFILLED (gap-filled v4.1.2
# product by default) are assigned -- left join to combined MSLSP, not inner join.
# Parcel-years with LandIQ crop rows but no MSLSP retrieval get assigned_by = "no_mslsp".
# - Non-woody primary: ADOY inside [OGI, OGMn]
# - Non-woody tie-break: nearest Peak to ADOY, then mslsp_cycle (1 before 2)
# - Woody: strongest remaining MSLSP cycle (cycle 1 first); do not use ADOY
# - CLASS-aware season priority:
#     * season 2 (main season) first when CLASS is present
#     * season 1 prioritized for MULTIUSE D/M (double/mixed-use; per LandIQ documentation)
#     * then seasons 3/4
#     * young woody (CLASS=YP or SPECOND=Y) after mature woody on the same parcel-year
#     * among the same priority, higher PCNT first
#
# MSLSP cycle convention (MSLSP User Guide V1, Table 1; BU-LCSC/MSLSP):
#   Cycle 1 = First Vegetation Cycle = largest EVI2 amplitude (dominant/strongest).
#   Cycle 2 = Second Vegetation Cycle = second largest EVI2 amplitude.
# Unused extra MSLSP cycles do not veto an in-window non-woody match in match_outcome.
# =====================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(dplyr)
  library(lubridate)
})

# --- Configuration ---
path_inventory  <- Sys.getenv("PRODUCTS_INVENTORY", "")
if (!nzchar(trimws(path_inventory))) {
  .root <- trimws(Sys.getenv("CCMMF_ROOT", ""))
  if (!nzchar(.root)) {
    stop("Set PRODUCTS_INVENTORY or CCMMF_ROOT (source documentation/setup_env.sh).")
  }
  path_inventory <- file.path(.root, "products", "inventory")
}
path_landiq_v4    <- Sys.getenv("LANDIQ_GAPFILLED", "")
if (!nzchar(trimws(path_landiq_v4))) {
  .root <- trimws(Sys.getenv("CCMMF_ROOT", ""))
  if (!nzchar(.root)) {
    stop("Set LANDIQ_GAPFILLED or CCMMF_ROOT (source documentation/setup_env.sh).")
  }
  path_landiq_v4 <- file.path(.root, "LandIQ", "gapfilled")
}
combined_root <- trimws(Sys.getenv("MSLSP_EXTRACT_ROOT", ""))
if (!nzchar(combined_root)) {
  nc <- trimws(Sys.getenv("MSLSP_NETCDF_ROOT", ""))
  if (!nzchar(nc)) {
    .root <- trimws(Sys.getenv("CCMMF_ROOT", ""))
    if (!nzchar(.root)) {
      stop("Set MSLSP_EXTRACT_ROOT, MSLSP_NETCDF_ROOT, or CCMMF_ROOT (source documentation/setup_env.sh).")
    }
    nc <- file.path(.root, "HLS", "MSLSP")
  }
  combined_root <- file.path(nc, "raw_mslsp_v4.1.2")
}
landiq_parq       <- file.path(path_landiq_v4, "crops_all_years.parq")
path_cropcode_csv <- function() {
  env <- trimws(Sys.getenv("LANDIQ_CROPCODE_CSV", ""))
  if (nzchar(env) && file.exists(env)) return(env)
  gf <- trimws(Sys.getenv("LANDIQ_GAPFILL_ROOT", ""))
  if (nzchar(gf)) {
    p <- file.path(gf, "data", "LandIQ_cropCode_lookup_table.csv")
    if (file.exists(p)) return(p)
  }
  code <- trimws(Sys.getenv("CCMMF_CODE", ""))
  if (nzchar(code)) {
    p <- file.path(code, "landiq-gapfill", "data", "LandIQ_cropCode_lookup_table.csv")
    if (file.exists(p)) return(p)
  }
  p <- file.path(path_inventory, "LandIQ_cropCode_lookup_table.csv")
  if (file.exists(p)) return(p)
  stop("LandIQ_cropCode_lookup_table.csv not found. Set LANDIQ_CROPCODE_CSV or LANDIQ_GAPFILL_ROOT.")
}

# Cores for the parcel assignment loop. MATCH_N_CORES wins; else scheduler
# slots; else min(8, detectCores()-1). Set MATCH_N_CORES=1 for serial.
match_n_cores <- function() {
  for (e in c("MATCH_N_CORES", "SLURM_CPUS_ON_NODE", "NSLOTS", "NCPUS")) {
    v <- suppressWarnings(as.integer(Sys.getenv(e, "")))
    if (!is.na(v) && v >= 1L) return(v)
  }
  n <- suppressWarnings(parallel::detectCores())
  if (is.na(n) || n < 2L) return(1L)
  min(8L, max(1L, n - 1L))
}

assign_field_years <- function(pids, yr, pheno, landiq, pheno_split, landiq_split) {
  n_cores <- match_n_cores()
  n_cores <- min(n_cores, length(pids))
  message("Assigning ", length(pids), " field-years on ", n_cores, " core(s)")
  assign_pids <- function(pids_i) {
    data.table::setDTthreads(1L)
    lapply(pids_i, function(pid) {
      cr <- pheno_split[[pid]]
      if (is.null(cr)) cr <- pheno[0L]
      lr <- landiq_split[[pid]]
      if (is.null(lr)) lr <- landiq[0L]
      assign_one_4rows(pid, yr, cr, lr)
    })
  }
  if (n_cores <= 1L || length(pids) < 64L || .Platform$OS.type != "unix") {
    return(assign_pids(pids))
  }
  chunks <- split(pids, cut(seq_along(pids), n_cores, labels = FALSE))
  chunk_res <- parallel::mclapply(chunks, assign_pids, mc.cores = n_cores)
  bad <- vapply(chunk_res, inherits, logical(1), "try-error")
  if (any(bad)) {
    stop("Assignment worker failed: ", as.character(chunk_res[[which(bad)[1L]]]))
  }
  unlist(chunk_res, recursive = FALSE, use.names = FALSE)
}

cropcode_csv      <- path_cropcode_csv()
.match_dir <- {
  fa <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(fa)) {
    dirname(normalizePath(sub("^--file=", "", fa[[1L]]), mustWork = FALSE))
  } else if (!is.null(sys.frame(1)$ofile)) {
    dirname(normalizePath(sys.frame(1)$ofile, mustWork = FALSE))
  } else {
    file.path(Sys.getenv("PHENOLOGY_ROOT", file.path(Sys.getenv("CCMMF_CODE"), "phenology")), "match")
  }
}
.mp <- file.path(.match_dir, "matched_paths.R")
if (!file.exists(.mp)) {
  .mp <- file.path(path_inventory, "scripts/phenology/matched_paths.R")
}
if (!file.exists(.mp)) {
  stop("matched_paths.R not found (set CCMMF_CODE / PHENOLOGY_ROOT).")
}
source(.mp)
out_dir           <- matched_landiq_dir(path_inventory)

eps_eviamp             <- 0.01
heterogeneity_na_frac_thr <- 0.5
assign_active_only     <- TRUE

year <- if (exists("YEAR", envir = .GlobalEnv)) get("YEAR", .GlobalEnv) else NULL
if (is.null(year) || !length(year) || is.na(suppressWarnings(as.integer(year[[1L]])))) {
  argv <- commandArgs(trailingOnly = TRUE)
  if (length(argv) >= 1L) year <- argv[[1L]]
}
year <- suppressWarnings(as.integer(year))
if (is.na(year)) stop("Usage: Rscript match_landiq_mslsp.R YEAR")
do_subset_test <- if (exists("DO_SUBSET_TEST", envir = .GlobalEnv)) get("DO_SUBSET_TEST", .GlobalEnv) else FALSE
sample_per_pft <- if (exists("SAMPLE_PER_PFT", envir = .GlobalEnv)) get("SAMPLE_PER_PFT", .GlobalEnv) else 500L
# DEMO_TILE: keep ag parcels that intersect that MGRS tile (training). ASSIGN_PARCEL_IDS_FILE:
# optional extra allowlist when DEMO_TILE is unset. Unset both for statewide.
demo_tile <- if (exists("DEMO_TILE", envir = .GlobalEnv)) get("DEMO_TILE", .GlobalEnv) else Sys.getenv("DEMO_TILE", "")
demo_tile <- trimws(as.character(demo_tile))
assign_parcel_ids_file <- if (exists("ASSIGN_PARCEL_IDS_FILE", envir = .GlobalEnv)) {
  get("ASSIGN_PARCEL_IDS_FILE", .GlobalEnv)
} else {
  Sys.getenv("ASSIGN_PARCEL_IDS_FILE", "")
}

hls_parcel_tilemap_path <- function() {
  lib <- trimws(Sys.getenv("HLS_SHARED_LIB", ""))
  if (!nzchar(lib)) {
    code <- trimws(Sys.getenv("CCMMF_CODE", ""))
    if (nzchar(code)) lib <- file.path(code, "hls", "R")
  }
  file.path(lib, "parcel_tilemap.R")
}

# Returns list(ids, out_path). Empty ids means keep every field-year (statewide).
match_subset_spec <- function(yr, out_path) {
  if (nzchar(demo_tile)) {
    p <- hls_parcel_tilemap_path()
    if (!file.exists(p)) {
      stop("DEMO_TILE is set but parcel_tilemap.R was not found. Set HLS_SHARED_LIB or CCMMF_CODE.")
    }
    source(p)
    ag <- load_parcel_tiles_for_year(yr, tile = demo_tile)
    ids <- unique(trimws(as.character(ag$parcel_id)))
    ids <- ids[nzchar(ids)]
    if (length(ids) == 0L) {
      stop(
        "No year ", yr, " ag parcels with tile_id=", demo_tile,
        " in ", path_parcel_tiles_csv()
      )
    }
    message("DEMO_TILE=", demo_tile, ": ", length(ids), " ag parcels from parcel_tiles")
    tile_dir <- paste0("tile=", demo_tile)
    out_sub <- if (identical(basename(out_path), tile_dir)) out_path else file.path(out_path, tile_dir)
    return(list(ids = ids, out_path = out_sub))
  }
  f <- trimws(assign_parcel_ids_file)
  if (nzchar(f) && file.exists(f)) {
    ids <- unique(trimws(as.character(fread(f)$parcel_id)))
    ids <- ids[nzchar(ids)]
    message("ASSIGN_PARCEL_IDS_FILE: ", length(ids), " parcel ids")
    out_sub <- if (identical(basename(out_path), "subsample_n400")) out_path else file.path(out_path, "subsample_n400")
    return(list(ids = ids, out_path = out_sub))
  }
  list(ids = character(0), out_path = out_path)
}

# --- Helpers ---
# Normalize DOY for same-year wrap (e.g. OGI=350, OGMn=50). Do NOT use for cross-year DOY.
norm_doy <- function(doy) {
  d <- as.numeric(doy)
  out <- d
  ok <- !is.na(d)
  out[ok & d < 1] <- d[ok & d < 1] + 365
  out[ok & d > 365] <- d[ok & d > 365] - 365
  out
}

# LandIQ: "negative ADOY = prior year (-92 = Oct 1); positive = target year" (metadata).
# MSLSP: DOY "January 1 of target year = 1", valid -181 to 548 (User Guide). Use date comparison.
adoy_in_window <- function(adoy, ogi, ogmn, year) {
  yr <- as.integer(year)[1]
  n <- max(length(adoy), length(ogi), length(ogmn))
  adoy <- rep(as.numeric(adoy), length.out = n)
  ogi  <- rep(as.numeric(ogi), length.out = n)
  ogmn <- rep(as.numeric(ogmn), length.out = n)
  out <- rep(NA, n)
  ok <- !is.na(adoy) & !is.na(ogi) & !is.na(ogmn)
  if (!any(ok)) return(out)
  d0 <- as.Date(sprintf("%d-01-01", yr))
  adoy_date <- d0 + as.integer(round(adoy[ok])) - 1L
  ogi_date  <- d0 + as.integer(round(ogi[ok])) - 1L
  ogmn_date <- d0 + as.integer(round(ogmn[ok])) - 1L
  lo <- pmin(ogi_date, ogmn_date)
  hi <- pmax(ogi_date, ogmn_date)
  out[ok] <- (adoy_date >= lo) & (adoy_date <= hi)
  out
}

is_real_cycle <- function(Peak, OGI, OGMn, w_valid, EVIamp, eps_amp = 0.01) {
  ok_dates <- !is.na(Peak) & !is.na(OGI) & !is.na(OGMn)
  ok_w <- !is.na(w_valid) & w_valid > 0
  ok_evi <- !is.na(EVIamp) & EVIamp > eps_amp
  ok_dates & ok_w & ok_evi
}

qc_mslsp_qa_pixel_agreement_flag <- function(gup_frac, gdown_frac, thr_good = 0.6, thr_bad = 0.3) {
  gf <- suppressWarnings(as.numeric(gup_frac))
  df <- suppressWarnings(as.numeric(gdown_frac))
  m <- pmax(gf, df, na.rm = TRUE)
  out <- rep("mixed_qa_pixel_agreement", length(m))
  out[is.na(gf) & is.na(df)] <- "qa_not_available"
  out[!is.na(m) & m >= thr_good] <- "high_qa_pixel_agreement"
  out[!is.na(m) & m <= thr_bad] <- "low_qa_pixel_agreement"
  out
}

heterogeneity_flag <- function(n_valid, w_valid, na_frac, na_frac_thr = 0.5) {
  out <- rep("low_na_frac", length(n_valid))
  out[is.na(n_valid) | is.na(w_valid)] <- "mslsp_metrics_missing"
  out[!is.na(na_frac) & na_frac >= na_frac_thr] <- "high_na_frac"
  out
}

qc_cycle_season_counts_label <- function(n_ms, n_li) {
  if (is.na(n_ms) || n_ms == 0) return("no_mslsp_data")
  if (is.na(n_li) || n_li == 0) return("no_landiq_active")
  paste0(n_ms, if (n_ms == 1L) "cycle" else "cycles", "_", n_li, if (n_li == 1L) "season" else "seasons")
}

qc_mslsp_pixel_availability_flag <- function(w_valid, n_valid) {
  has <- !is.na(w_valid) & w_valid > 0 & !is.na(n_valid) & n_valid > 0
  out <- rep("no_pixels_or_invalid", length(w_valid))
  out[has] <- "has_pixels"
  out[is.na(w_valid) & is.na(n_valid)] <- "no_mslsp_data_for_cycle"
  out
}

date_dist_days <- function(yr, doy1, doy2) {
  n <- max(length(yr), length(doy1), length(doy2))
  yr <- rep(as.integer(yr), length.out = n)
  doy1 <- rep(as.numeric(doy1), length.out = n)
  doy2 <- rep(as.numeric(doy2), length.out = n)
  ok <- !is.na(yr) & !is.na(doy1) & !is.na(doy2)
  out <- rep(NA_real_, n)
  if (!any(ok)) return(out)
  d0 <- as.Date(sprintf("%d-01-01", yr[ok]))
  d1 <- d0 + as.integer(round(doy1[ok])) - 1L
  d2 <- d0 + as.integer(round(doy2[ok])) - 1L
  out[ok] <- as.numeric(d1 - d2)
  out
}

# Convert DOY to Date (handles cross-year: doy < 1 = prior year, doy > 366 = next year)
doy_to_date <- function(year, doy) {
  yr <- as.integer(year)[1]
  d <- suppressWarnings(as.numeric(doy)[1])
  if (is.na(yr) || is.na(d)) return(as.Date(NA))
  lubridate::ymd(paste0(yr, "-01-01")) + lubridate::days(round(d) - 1L)
}

load_combined_mslsp <- function(year, root = combined_root) {
  year <- as.integer(year)
  stopifnot("year must be a valid integer" = !is.na(year))
  path <- file.path(root, paste0("year=", year), sprintf("mslsp_year=%d.parquet", year))
  if (!file.exists(path)) stop("Combined Parquet not found: ", path)
  d <- as.data.table(arrow::read_parquet(path))
  stopifnot("Combined file is empty" = nrow(d) > 0L)
  stopifnot("Combined data must have parcel_id" = "parcel_id" %in% names(d))
  stopifnot("Combined data must have cycle column" = "cycle" %in% names(d))
  d[, parcel_id := as.character(parcel_id)]
  d[, year := as.integer(year)]
  setorder(d, parcel_id, year, cycle)
  d
}

col1 <- function(row, name) {
  v <- row[[name]]
  if (!is.null(v)) return(v[1])
  v <- row[[paste0(name, ".x")]]
  if (!is.null(v)) return(v[1])
  NA_real_
}
opt <- function(row, name) { v <- row[[name]]; if (is.null(v)) NA_real_ else v[1] }

mslsp_long_to_cycles <- function(long_rows) {
  if (nrow(long_rows) == 0) return(list(mslsp_cycles = NULL, keep_reason = "no_mslsp_pixels"))
  pid <- long_rows$parcel_id[1]
  yr <- long_rows$year[1]
  fill <- function(r, prefix) {
    m <- r[[paste0(prefix, "_mean")]]
    if (is.null(m)) NA_real_ else m
  }
  mslsp_cycle_list <- vector("list", nrow(long_rows))
  for (j in seq_len(nrow(long_rows))) {
    r <- long_rows[j]
    cyc <- as.integer(r$cycle[1])
    if (is.na(cyc)) cyc <- j
    mslsp_cycle_list[[j]] <- data.table(
      parcel_id = pid, year = yr, mslsp_cycle = cyc, mslsp_rank = j,
      mslsp_OGI = fill(r, "OGI"), mslsp_50PCGI = fill(r, "50PCGI"), mslsp_OGMx = fill(r, "OGMx"), mslsp_Peak = fill(r, "Peak"),
      mslsp_OGD = fill(r, "OGD"), mslsp_50PCGD = fill(r, "50PCGD"), mslsp_OGMn = fill(r, "OGMn"),
      mslsp_EVImax = fill(r, "EVImax"), mslsp_EVIamp = fill(r, "EVIamp"), mslsp_EVIarea = fill(r, "EVIarea"),
      mslsp_w_valid = col1(r, "w_valid"), mslsp_n_valid = as.integer(col1(r, "n_valid")), mslsp_na_frac = col1(r, "na_frac"),
      mslsp_numObs = opt(r, "numObs_mean"), mslsp_NumCycles = opt(r, "NumCycles_mode"),
      mslsp_gupQA_mode = opt(r, "gupQA_mode"), mslsp_gupQA_mode_frac = opt(r, "gupQA_mode_frac"),
      mslsp_gdownQA_mode = opt(r, "gdownQA_mode"), mslsp_gdownQA_mode_frac = opt(r, "gdownQA_mode_frac")
    )
  }
  mslsp_cycles <- rbindlist(mslsp_cycle_list)
  mslsp_cycles[, cycle_real := is_real_cycle(mslsp_Peak, mslsp_OGI, mslsp_OGMn, mslsp_w_valid, mslsp_EVIamp, eps_amp = eps_eviamp)]
  real <- mslsp_cycles[cycle_real == TRUE]
  if (nrow(real) == 0) return(list(mslsp_cycles = NULL, keep_reason = "mslsp_cycles_filtered_out"))
  real[, mslsp_rank := seq_len(.N)]
  list(mslsp_cycles = real, keep_reason = if (nrow(real) >= 2) "both_mslsp_cycles_kept" else "single_mslsp_cycle_only")
}

is_valid_landiq_class <- function(x) {
  x <- trimws(as.character(x))
  !is.na(x) & nzchar(x) & x != "**"
}

is_woody_pft <- function(pft) {
  tolower(trimws(as.character(pft))) == "woody"
}

is_young_woody <- function(pft, class_code, specond = NA_character_) {
  is_woody_pft(pft) & (
    toupper(trimws(as.character(class_code))) == "YP" |
      toupper(trimws(as.character(specond))) == "Y"
  )
}

season_component_priority <- function(season, class_code, multiuse = NA_character_,
                                      pft = NA_character_, specond = NA_character_) {
  s <- suppressWarnings(as.integer(season))
  class_ok <- is_valid_landiq_class(class_code)
  mu <- trimws(as.character(multiuse))
  is_distinct_or_mixed <- !is.na(mu) & mu %in% c("D", "M", "d", "m")

  out <- rep(99L, length(s))
  out[class_ok & s == 2L] <- 1L
  out[class_ok & s == 1L & is_distinct_or_mixed] <- 2L
  out[class_ok & s == 1L & !is_distinct_or_mixed] <- 3L
  out[class_ok & s == 3L] <- 4L
  out[class_ok & s == 4L] <- 5L
  out[!class_ok] <- 20L + fifelse(is.na(s), 9L, s)
  young <- is_young_woody(pft, class_code, specond)
  young[is.na(young)] <- FALSE
  out[young] <- out[young] + 10L
  out
}

assignment_class_rollup <- function(DT) {
  first_nonempty <- function(x) {
    y <- as.character(x)
    y <- y[!is.na(y) & nzchar(trimws(y))]
    if (length(y) == 0) NA_character_ else y[1]
  }
  if (any(DT$qc_cycle_season_counts == "no_mslsp_pixels", na.rm = TRUE)) return("no_mslsp_pixels")
  if (any(DT$qc_cycle_season_counts == "mslsp_cycles_filtered_out", na.rm = TRUE)) return("mslsp_cycles_filtered_out")
  if (any(DT$qc_cycle_season_counts == "no_landiq_active", na.rm = TRUE)) return("no_landiq_active")
  if (any(DT$qc_cycle_season_counts == "no_mslsp_cycle_for_season", na.rm = TRUE)) return("no_mslsp_cycle_assigned")

  matched <- DT[assigned_by == "matched"]
  if (nrow(matched) == 0L) return("no_mslsp_cycle_assigned")

  if (any(is_woody_pft(matched$landiq_PFT) & matched$assigned_woody_tiebreak %in% TRUE, na.rm = TRUE)) {
    return("matched_woody_strongest_cycle")
  }
  if (any(matched$qc_n_adoy_in_cycle == "multiple_adoy_in_cycle", na.rm = TRUE)) {
    return("multiple_adoy_in_cycle_review")
  }
  # In-window assigned season is validated even if MSLSP kept an unused extra cycle.
  if (any(matched$qc_adoy_vs_cycle == "adoy_inside_cycle", na.rm = TRUE)) {
    return("adoy_inside_and_single")
  }
  if (any(matched$qc_adoy_vs_cycle == "adoy_outside_cycle", na.rm = TRUE)) {
    return("adoy_outside_cycle_review")
  }

  cc <- first_nonempty(matched$qc_cycle_season_counts)
  if (!is.na(cc) && !(cc %in% c("1cycle_1season", "2cycles_2seasons"))) {
    if (cc == "2cycles_1season") return("mismatch_2cycles_1season")
    if (cc == "1cycle_2seasons") return("mismatch_1cycle_2seasons")
    return(paste0("mismatch_", cc))
  }
  if (is.na(cc)) return("mismatch_unclassified")
  return("matched_no_adoy")
}

assign_one_4rows <- function(pid, yr, combined_row, landiq_rows) {
  out <- data.table(
    parcel_id = pid, year = yr, season = 1L:4L,
    assigned_by = "no_match", assigned_woody_tiebreak = FALSE,
    mslsp_cycle = NA_integer_,
    landiq_PCNT = NA_real_, landiq_ADOY = NA_real_, landiq_PFT = NA_character_,
    landiq_CLASS = NA_character_, landiq_SUBCLASS = NA_character_, landiq_SPECOND = NA_character_,
    landiq_MULTIUSE = NA_character_, landiq_COVER = FALSE,
    landiq_adoy_source = NA_character_,
    mslsp_Peak = as.Date(NA), mslsp_OGI = as.Date(NA), mslsp_OGMn = as.Date(NA),
    mslsp_50PCGI = as.Date(NA), mslsp_OGMx = as.Date(NA), mslsp_OGD = as.Date(NA), mslsp_50PCGD = as.Date(NA),
    mslsp_EVImax = NA_real_, mslsp_EVIamp = NA_real_, mslsp_EVIarea = NA_real_,
    mslsp_w_valid = NA_real_, mslsp_n_valid = NA_integer_, mslsp_na_frac = NA_real_, mslsp_numObs = NA_real_, mslsp_NumCycles = NA_real_,
    peak_dist_days = NA_real_,
    mslsp_gupQA_mode = NA_real_, mslsp_gupQA_mode_frac = NA_real_, mslsp_gdownQA_mode = NA_real_, mslsp_gdownQA_mode_frac = NA_real_,
    qc_landiq_season_data = NA_character_,
    qc_adoy_vs_cycle = NA_character_, qc_n_adoy_in_cycle = NA_character_,
    qc_adoy_status = NA_character_, qc_cycle_status = NA_character_,
    qc_adoy_cycle_relation = NA_character_, qc_adoy_multiplicity = NA_character_,
    qc_mslsp_pixel_availability = NA_character_, qc_heterogeneity = NA_character_,
    qc_cycle_season_counts = NA_character_, qc_mslsp_qa_pixel_agreement = NA_character_,
    match_outcome = NA_character_, qc_mslsp_cycles_available = NA_character_
  )

  for (s in 1:4) {
    r <- landiq_rows[season == s]
    if (nrow(r)) {
      out[season == s, `:=`(
        landiq_PCNT = r$PCNT[1], landiq_ADOY = r$ADOY[1], landiq_PFT = r$PFT[1],
        landiq_CLASS = if ("CLASS" %in% names(r)) r$CLASS[1] else NA_character_,
        landiq_SUBCLASS = if ("SUBCLASS" %in% names(r)) r$SUBCLASS[1] else NA_character_,
        landiq_SPECOND = if ("SPECOND" %in% names(r)) trimws(as.character(r$SPECOND[1])) else NA_character_,
        landiq_MULTIUSE = if ("MULTIUSE" %in% names(r)) r$MULTIUSE[1] else NA_character_,
        landiq_adoy_source = if ("adoy_source" %in% names(r)) {
          trimws(as.character(r$adoy_source[1]))
        } else {
          NA_character_
        },
        landiq_COVER = if ("COVER" %in% names(r)) {
          isTRUE(as.logical(r$COVER[1]))
        } else {
          FALSE
        }
      )]
      pcnt <- r$PCNT[1]
      out[season == s, qc_landiq_season_data := fifelse(!is.na(pcnt) & pcnt >= 0, "landiq_season_has_data", NA_character_)]
    } else {
      out[season == s, qc_landiq_season_data := NA_character_]
    }
  }

  active <- landiq_rows[!is.na(PCNT) & PCNT >= 0]
  n_landiq_active <- nrow(active)
  if (n_landiq_active == 0) {
    out[, `:=`(
      qc_adoy_vs_cycle = NA_character_, qc_n_adoy_in_cycle = NA_character_,
      qc_mslsp_pixel_availability = NA_character_, qc_heterogeneity = NA_character_,
      qc_mslsp_qa_pixel_agreement = NA_character_,
      qc_cycle_season_counts = "no_landiq_active",
      qc_mslsp_cycles_available = NA_character_
    )]
    out[, match_outcome := "no_landiq_active"]
    return(list(assigned = out))
  }

  cyc <- mslsp_long_to_cycles(combined_row)
  mslsp_cycles <- cyc$mslsp_cycles
  if (is.null(mslsp_cycles) || nrow(mslsp_cycles) == 0) {
    out[, `:=`(
      qc_adoy_vs_cycle = NA_character_, qc_n_adoy_in_cycle = NA_character_,
      qc_mslsp_pixel_availability = NA_character_, qc_heterogeneity = NA_character_,
      qc_mslsp_qa_pixel_agreement = NA_character_,
      qc_cycle_season_counts = cyc$keep_reason,
      qc_mslsp_cycles_available = cyc$keep_reason
    )]
    if (identical(cyc$keep_reason, "no_mslsp_pixels")) {
      out[, assigned_by := "no_mslsp"]
    }
    out[, match_outcome := cyc$keep_reason]
    return(list(assigned = out))
  }

  n_mslsp_used <- nrow(mslsp_cycles)
  out[, qc_cycle_season_counts := qc_cycle_season_counts_label(n_mslsp_used, n_landiq_active)]

  landiq_active <- active[, .(
    landiq_season = season,
    landiq_ADOY = ADOY,
    landiq_PFT = if ("PFT" %in% names(active)) PFT else NA_character_,
    landiq_CLASS = if ("CLASS" %in% names(active)) CLASS else NA_character_,
    landiq_SUBCLASS = if ("SUBCLASS" %in% names(active)) SUBCLASS else NA_character_,
    landiq_MULTIUSE = if ("MULTIUSE" %in% names(active)) MULTIUSE else NA_character_,
    landiq_SPECOND = if ("SPECOND" %in% names(active)) {
      trimws(as.character(SPECOND))
    } else {
      NA_character_
    }
  )]
  landiq_active[, landiq_PCNT := active$PCNT]
  landiq_active[, component_priority := season_component_priority(
    landiq_season, landiq_CLASS, landiq_MULTIUSE, landiq_PFT, landiq_SPECOND
  )]
  landiq_active[, pcnt_ord := fifelse(is.na(landiq_PCNT), 0, as.numeric(landiq_PCNT))]

  mslsp_cycles[, keytmp := 1L]
  landiq_active[, keytmp := 1L]
  combos <- merge(mslsp_cycles, landiq_active, by = "keytmp", allow.cartesian = TRUE)
  combos[, keytmp := NULL]
  combos[, has_adoy := !is.na(landiq_ADOY) & !is.na(mslsp_OGI) & !is.na(mslsp_OGMn)]
  combos[, adoy_in_window := fifelse(has_adoy, adoy_in_window(landiq_ADOY, mslsp_OGI, mslsp_OGMn, yr), NA)]
  combos[, peak_dist_abs := fifelse(has_adoy, abs(as.numeric(date_dist_days(yr, mslsp_Peak, landiq_ADOY))), Inf)]
  combos[is.na(peak_dist_abs), peak_dist_abs := Inf]
  combos[, is_woody := is_woody_pft(landiq_PFT)]

  season_order <- landiq_active[order(component_priority, -pcnt_ord, landiq_season), unique(landiq_season)]
  used_cycles <- integer()
  chosen_rows <- vector("list", length(season_order))
  n_chosen <- 0L
  for (sea in season_order) {
    cand <- combos[landiq_season == sea & !(mslsp_cycle %in% used_cycles)]
    if (nrow(cand) == 0) next

    woody <- any(cand$is_woody, na.rm = TRUE)
    if (isTRUE(woody)) {
      setorder(cand, mslsp_cycle)
      pick <- cand[1]
      pick[, assigned_woody_tiebreak := TRUE]
    } else {
      in_window <- cand[adoy_in_window == TRUE]
      if (nrow(in_window) > 0) {
        setorder(in_window, peak_dist_abs, mslsp_cycle)
        pick <- in_window[1]
        pick[, assigned_woody_tiebreak := FALSE]
      } else {
        setorder(cand, peak_dist_abs, mslsp_cycle)
        pick <- cand[1]
        pick[, assigned_woody_tiebreak := FALSE]
      }
    }

    n_chosen <- n_chosen + 1L
    chosen_rows[[n_chosen]] <- pick
    used_cycles <- c(used_cycles, as.integer(pick$mslsp_cycle[1]))
  }

  chosen <- if (n_chosen > 0) rbindlist(chosen_rows[seq_len(n_chosen)], use.names = TRUE, fill = TRUE) else combos[0]

  landiq_adoy_vec <- active$ADOY
  n_in_cycle <- vapply(seq_len(nrow(chosen)), function(i) {
    sum(adoy_in_window(landiq_adoy_vec, chosen$mslsp_OGI[i], chosen$mslsp_OGMn[i], yr), na.rm = TRUE)
  }, integer(1))
  chosen[, n_adoy_in_cycle := n_in_cycle]
  chosen[has_adoy == TRUE & adoy_in_window == TRUE, n_adoy_in_cycle := pmax(n_adoy_in_cycle, 1L)]
  chosen[, qc_n_adoy_in_cycle := fifelse(!has_adoy, "no_adoy_recorded",
    fifelse(is.na(n_adoy_in_cycle) | n_adoy_in_cycle == 0L, "no_adoy_in_cycle",
      fifelse(n_adoy_in_cycle == 1L, "one_adoy_in_cycle", "multiple_adoy_in_cycle")))]
  chosen[, assigned_woody_tiebreak := if ("assigned_woody_tiebreak" %in% names(chosen)) assigned_woody_tiebreak else FALSE]
  chosen[, qc_adoy_vs_cycle := fcase(
    assigned_woody_tiebreak == TRUE, "woody_strongest_cycle",
    has_adoy & adoy_in_window == TRUE, "adoy_inside_cycle",
    has_adoy & (is.na(adoy_in_window) | !adoy_in_window), "adoy_outside_cycle",
    default = "no_adoy_recorded"
  )]
  chosen[, qc_mslsp_pixel_availability := qc_mslsp_pixel_availability_flag(mslsp_w_valid, mslsp_n_valid)]
  chosen[, qc_heterogeneity := heterogeneity_flag(mslsp_n_valid, mslsp_w_valid, mslsp_na_frac, na_frac_thr = heterogeneity_na_frac_thr)]
  chosen[, qc_mslsp_qa_pixel_agreement := qc_mslsp_qa_pixel_agreement_flag(mslsp_gupQA_mode_frac, mslsp_gdownQA_mode_frac)]
  chosen[, qc_cycle_season_counts := qc_cycle_season_counts_label(n_mslsp_used, n_landiq_active)]
  chosen[, qc_mslsp_cycles_available := cyc$keep_reason]

  for (k in seq_len(nrow(chosen))) {
    sea <- chosen$landiq_season[k]
    rr <- which(out$season == sea)
    if (length(rr) != 1) next
    out[rr, `:=`(
      assigned_by = "matched",
      assigned_woody_tiebreak = chosen$assigned_woody_tiebreak[k],
      mslsp_cycle = chosen$mslsp_cycle[k],
      mslsp_OGI = doy_to_date(yr, chosen$mslsp_OGI[k]), mslsp_50PCGI = doy_to_date(yr, chosen$mslsp_50PCGI[k]), mslsp_OGMx = doy_to_date(yr, chosen$mslsp_OGMx[k]),
      mslsp_Peak = doy_to_date(yr, chosen$mslsp_Peak[k]), mslsp_OGD = doy_to_date(yr, chosen$mslsp_OGD[k]), mslsp_50PCGD = doy_to_date(yr, chosen$mslsp_50PCGD[k]), mslsp_OGMn = doy_to_date(yr, chosen$mslsp_OGMn[k]),
      mslsp_EVImax = chosen$mslsp_EVImax[k], mslsp_EVIamp = chosen$mslsp_EVIamp[k], mslsp_EVIarea = chosen$mslsp_EVIarea[k],
      mslsp_w_valid = chosen$mslsp_w_valid[k], mslsp_n_valid = chosen$mslsp_n_valid[k], mslsp_na_frac = chosen$mslsp_na_frac[k],
      mslsp_numObs = chosen$mslsp_numObs[k],
      peak_dist_days = chosen$peak_dist_abs[k],
      mslsp_gupQA_mode = chosen$mslsp_gupQA_mode[k], mslsp_gupQA_mode_frac = chosen$mslsp_gupQA_mode_frac[k],
      mslsp_gdownQA_mode = chosen$mslsp_gdownQA_mode[k], mslsp_gdownQA_mode_frac = chosen$mslsp_gdownQA_mode_frac[k],
      qc_adoy_vs_cycle = chosen$qc_adoy_vs_cycle[k],
      qc_n_adoy_in_cycle = chosen$qc_n_adoy_in_cycle[k],
      qc_mslsp_pixel_availability = qc_mslsp_pixel_availability_flag(chosen$mslsp_w_valid[k], chosen$mslsp_n_valid[k]),
      qc_heterogeneity = heterogeneity_flag(chosen$mslsp_n_valid[k], chosen$mslsp_w_valid[k], chosen$mslsp_na_frac[k], na_frac_thr = heterogeneity_na_frac_thr),
      qc_mslsp_qa_pixel_agreement = qc_mslsp_qa_pixel_agreement_flag(chosen$mslsp_gupQA_mode_frac[k], chosen$mslsp_gdownQA_mode_frac[k])
    )]
  }

  out[assigned_by == "no_match", `:=`(
    qc_adoy_vs_cycle = NA_character_, qc_n_adoy_in_cycle = NA_character_,
    qc_mslsp_pixel_availability = NA_character_, qc_heterogeneity = NA_character_,
    qc_mslsp_qa_pixel_agreement = NA_character_,
    qc_cycle_season_counts = NA_character_, qc_mslsp_cycles_available = NA_character_
  )]
  out[assigned_by == "no_match" & qc_landiq_season_data == "landiq_season_has_data", `:=`(
    qc_adoy_vs_cycle = "no_cycle_assigned",
    qc_n_adoy_in_cycle = "no_cycle_assigned",
    qc_mslsp_pixel_availability = "no_mslsp_data_for_cycle",
    qc_heterogeneity = "no_cycle_assigned",
    qc_mslsp_qa_pixel_agreement = "no_cycle_assigned",
    qc_cycle_season_counts = "no_mslsp_cycle_for_season",
    qc_mslsp_cycles_available = "no_cycle_assigned"
  )]

  out[, qc_adoy_status := fifelse(
    qc_landiq_season_data == "landiq_season_has_data",
    fifelse(is.na(landiq_ADOY), "no_landiq_adoy", "has_adoy"),
    NA_character_
  )]
  out[, qc_cycle_status := fifelse(
    qc_landiq_season_data == "landiq_season_has_data",
    fifelse(assigned_by == "matched", "has_assigned_cycle", "no_assigned_cycle"),
    NA_character_
  )]
  out[, qc_adoy_cycle_relation := fifelse(
    qc_adoy_status == "has_adoy" & qc_cycle_status == "has_assigned_cycle",
    qc_adoy_vs_cycle,
    NA_character_
  )]
  out[, qc_adoy_multiplicity := fifelse(
    qc_adoy_status == "has_adoy" & qc_cycle_status == "has_assigned_cycle",
    qc_n_adoy_in_cycle,
    NA_character_
  )]

  if ("NumCycles_mode" %in% names(combined_row)) out[, mslsp_NumCycles := combined_row$NumCycles_mode[1]]
  out[assigned_by == "matched", qc_mslsp_cycles_available := cyc$keep_reason]

  list(assigned = out)
}

# Extract QC summary from assigned data (file path or data.table).
# Returns long-format: year, level, qc_dimension, category, pft, n, pct.
# qc_mslsp_cycles_available only at field_year (avoids redundant row-level copy).
# If out_dir is set, writes qc_summary_year=Y.csv.
extract_qc_summary <- function(assigned_path_or_dt, out_dir = NULL) {
  if (is.character(assigned_path_or_dt)) {
    if (!file.exists(assigned_path_or_dt)) stop("File not found: ", assigned_path_or_dt)
    assigned <- as.data.table(arrow::read_parquet(assigned_path_or_dt))
    yr <- as.integer(assigned$year[1])
  } else {
    assigned <- as.data.table(assigned_path_or_dt)
    yr <- as.integer(assigned$year[1])
  }
  pft_col <- names(assigned)[tolower(names(assigned)) %in% c("pft", "landiq_pft")][1]
  has_pft <- !is.na(pft_col)

  matched <- assigned[assigned_by == "matched"]
  row_dims <- c(
    "qc_adoy_vs_cycle", "qc_n_adoy_in_cycle", "qc_cycle_season_counts",
    "qc_mslsp_pixel_availability", "qc_heterogeneity", "qc_mslsp_qa_pixel_agreement"
  )
  row_dims <- intersect(row_dims, names(matched))
  out <- data.table(year = integer(), level = character(), qc_dimension = character(), category = character(), pft = character(), n = integer(), pct = double())
  if (nrow(matched) > 0 && length(row_dims) > 0) {
    for (d in row_dims) {
      by_cols <- c(d, if (has_pft) pft_col else NULL)
      tab <- matched[, .N, by = by_cols]
      tab[, category := fifelse(is.na(get(d)), "(no value)", as.character(get(d)))]
      tab[, pft := if (has_pft) fifelse(is.na(get(pft_col)), "(no value)", as.character(get(pft_col))) else "(all)"]
      if (has_pft) tab[, (pft_col) := NULL]
      tab[, (d) := NULL]
      tab[, pct := round(100 * N / sum(N), 1), by = "pft"]
      setnames(tab, "N", "n")
      tab[, `:=`(year = yr, level = "row", qc_dimension = d)]
      setcolorder(tab, c("year", "level", "qc_dimension", "category", "pft", "n", "pct"))
      out <- rbind(out, tab)
    }
  }
  fy_cols <- c("match_outcome", "qc_mslsp_cycles_available", if (has_pft) pft_col else NULL)
  fy_rollup <- assigned[, lapply(.SD, function(x) x[!is.na(x)][1L]), by = .(parcel_id, year), .SDcols = fy_cols]
  if (has_pft) setnames(fy_rollup, pft_col, "pft_rollup")
  n_fy <- nrow(fy_rollup)
  if (n_fy > 0) {
    for (d in c("match_outcome", "qc_mslsp_cycles_available")) {
      if (!d %in% names(fy_rollup)) next
      by_fy <- c(d, if (has_pft) "pft_rollup" else NULL)
      tab <- fy_rollup[, .N, by = by_fy]
      tab[, category := fifelse(is.na(get(d)), "(no value)", as.character(get(d)))]
      tab[, pft := if (has_pft) fifelse(is.na(pft_rollup), "(no value)", as.character(pft_rollup)) else "(all)"]
      if (has_pft) tab[, pft_rollup := NULL]
      tab[, (d) := NULL]
      if (has_pft) tab[, pct := round(100 * N / sum(N), 1), by = "pft"] else tab[, pct := round(100 * N / n_fy, 1)]
      setnames(tab, "N", "n")
      tab[, `:=`(year = yr, level = "field_year", qc_dimension = d)]
      setcolorder(tab, c("year", "level", "qc_dimension", "category", "pft", "n", "pct"))
      out <- rbind(out, tab)
    }
  }
  if (nrow(out) > 0) {
    # Sort: level (row then field_year), qc_dimension (fixed order), pft (row/hay/rice/woody/noncrop then other), then n descending
    pft_order <- c("row", "hay", "rice", "woody", "noncrop", "other", "(no value)", "(all)")
    dim_order <- c(
      "qc_adoy_vs_cycle", "qc_n_adoy_in_cycle", "qc_cycle_season_counts",
      "qc_mslsp_pixel_availability", "qc_heterogeneity", "qc_mslsp_qa_pixel_agreement",
      "match_outcome", "qc_mslsp_cycles_available"
    )
    out[, level := factor(level, levels = c("row", "field_year"))]
    out[, qc_dimension := factor(qc_dimension, levels = dim_order)]
    out[, pft := factor(pft, levels = c(pft_order, setdiff(unique(out$pft), pft_order)))]
    setorder(out, level, qc_dimension, pft, -n)
    out[, level := as.character(level)]
    out[, qc_dimension := as.character(qc_dimension)]
    out[, pft := as.character(pft)]
    if (length(out_dir) > 0 && nzchar(out_dir)) {
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
      out_file <- file.path(out_dir, paste0("qc_summary_year=", yr, ".csv"))
      fwrite(out, out_file)
      message("QC summary: ", out_file)
    }
  }
  invisible(out)
}

summarize_assignment <- function(year, out_path = out_dir) {
  yr <- as.integer(year)
  assigned_path <- file.path(out_path, paste0("assigned_year=", yr, ".parquet"))
  if (!file.exists(assigned_path)) stop("assigned parquet not found: ", assigned_path)
  extract_qc_summary(assigned_path, out_path)
}

run_assignment <- function(year, cr = combined_root,
                           subset_test = do_subset_test, samp_per_pft = sample_per_pft,
                           out_path = out_dir) {
  yr <- as.integer(year)
  stopifnot("year must be a valid integer" = !is.na(yr))
  spec <- match_subset_spec(yr, out_path)
  out_path <- spec$out_path
  ids <- spec$ids

  message("Loading combined MSLSP year=", yr)
  pheno <- load_combined_mslsp(yr, cr)
  if (!"MULTIUSE" %in% names(pheno)) pheno[, MULTIUSE := NA_character_]
  if (length(ids) > 0L) pheno <- pheno[parcel_id %in% ids]

  message("Loading LandIQ year=", yr,
          if (length(ids) > 0L) paste0(" (DEMO_TILE parcels only)") else "")
  lookup   <- fread(cropcode_csv)
  ag_pairs <- unique(lookup[is_agricultural == TRUE, .(CLASS = trimws(CLASS), SUBCLASS = as.character(SUBCLASS), PFT)])
  ag_classes <- unique(ag_pairs$CLASS)
  landiq <- as.data.table(
    arrow::open_dataset(landiq_parq) |>
      dplyr::filter(year == !!yr, CLASS %in% !!ag_classes) |>
      dplyr::collect()
  )
  landiq[, parcel_id := trimws(as.character(parcel_id))]
  if (length(ids) > 0L) {
    landiq <- landiq[parcel_id %in% ids]
  }
  landiq[, CLASS := trimws(as.character(CLASS))]
  landiq[, SUBCLASS := as.character(SUBCLASS)]
  # Normalize "no subclass" so CLASS X (fallow) and other ** in lookup match: NA, "", "**" -> "**"
  landiq[is.na(SUBCLASS) | trimws(SUBCLASS) == "" | trimws(SUBCLASS) == "**", SUBCLASS := "**"]
  if ("MULTIUSE" %in% names(landiq)) landiq[, MULTIUSE := trimws(as.character(MULTIUSE))]
  landiq[, PCNT := suppressWarnings(as.numeric(PCNT))]
  landiq[, ADOY := suppressWarnings(as.numeric(ADOY))]
  landiq[ADOY == 0, ADOY := NA_real_]
  landiq[, year := as.integer(year)]
  landiq <- merge(landiq, ag_pairs, by = c("CLASS", "SUBCLASS"))
  setkey(landiq, parcel_id, year)
  setkey(pheno, parcel_id, year)
  fys <- if (isTRUE(assign_active_only)) {
    unique(landiq[!is.na(PCNT) & PCNT >= 0, .(parcel_id, year)])
  } else {
    unique(landiq[, .(parcel_id, year)])
  }
  if (nrow(fys) == 0) {
    if (length(ids) > 0L) {
      message("No LandIQ field-years in this subset for year ", yr, "; skipping.")
      return(invisible(list(assigned = data.table())))
    }
    stop("No LandIQ field-years for year ", yr)
  }
  if (isTRUE(subset_test) && length(ids) == 0L) {
    n_take <- min(samp_per_pft, nrow(fys))
    fys <- fys[sample(.N, n_take)]
  }
  setorder(fys, parcel_id, year)
  n_with_mslsp <- uniqueN(fys[pheno, on = c("parcel_id", "year"), nomatch = 0, parcel_id])
  message(
    "LandIQ field-years: ", nrow(fys),
    "; with MSLSP: ", n_with_mslsp,
    "; without MSLSP: ", nrow(fys) - n_with_mslsp
  )
  if (n_with_mslsp == 0) {
    warning("No overlap with combined MSLSP for year ", yr, "; all rows will be assigned_by=no_mslsp")
  }
  pheno_split  <- split(pheno, pheno$parcel_id)
  landiq_split <- split(landiq, landiq$parcel_id)
  results <- assign_field_years(fys$parcel_id, yr, pheno, landiq, pheno_split, landiq_split)
  assigned <- rbindlist(lapply(results, `[[`, "assigned"), fill = TRUE)
  assigned[, match_outcome := {
    ac <- assignment_class_rollup(.SD)
    fcase(ac == "matched_no_adoy", "matched_no_adoy", ac == "adoy_inside_and_single", "matched_adoy_validated", default = ac)
  }, by = .(parcel_id, year)]
  assigned[assigned_by == "no_match" & (is.na(qc_landiq_season_data) | qc_landiq_season_data != "landiq_season_has_data"), match_outcome := NA_character_]
  dir.create(out_path, recursive = TRUE, showWarnings = FALSE)
  out_assigned <- file.path(out_path, paste0("assigned_year=", yr, ".parquet"))
  arrow::write_parquet(assigned, out_assigned)
  message("Wrote ", out_assigned, " rows=", nrow(assigned))
  extract_qc_summary(assigned, out_path)
  invisible(list(assigned = assigned))
}

if (!is.null(year)) {
  run_assignment(year, subset_test = do_subset_test, samp_per_pft = sample_per_pft)
}

