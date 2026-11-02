# =====================================================================================
# 01_historical_model.R : build, validate and calibrate the model on 2018-2023 data.
#
# Model: for each parcel and season, find the anchor date (start of the season) from
# daily weather, then add up a daily score (heat + dry day + dry soil) after the anchor.
# The date the running total reaches a threshold Fcrit is the predicted planting date
# (onset of greenness increase, OGI) or harvest date (onset of greenness minimum, OGMn).
# Fcrit is calibrated separately for each crop class x anchor x segment.
#
# Steps
#   1 Daymet temperature and precipitation -> cell x day matrices
#   2 MSLSP phenology matched to LandIQ parcels -> one row per parcel-season
#   3 daily score matrix
#   4 anchors ("c7" fixed 7 C threshold, "rel" season-relative threshold)
#   5 tables of observed days from anchor to planting / harvest
#   6 leave-one-year-out validation vs a climatology baseline
#   7 final Fcrit calibration on all validation years
#   8 forward-in-time validation (train on earlier years only)
#   9 validated class lists
#
# Run time is long (validation is the slow part); run it as a batch job on the SCC.
# Outputs (rds, in <base_dir>/rebuild_outputs):
#   calibrated_Fcrit.rds, validation_by_year.rds, validation_summary.rds,
#   validation_forward_by_year.rds, validation_forward_summary.rds, validated_pairs.rds,
#   parcel_classes.rds, anchor_diagnostics.rds
# =====================================================================================

source("00_setup.R")
suppressPackageStartupMessages({ library(readxl); library(arrow); library(terra) })
set.seed(42)    # makes the calibration subsample repeatable (keep the order of sections 6 and 7)

# ---------------------------------------------------------------- 1. DAYMET CLIMATE
daymet_stack <- function(var, folder) {
  lst <- lapply(yrs_all, function(y) {
    f <- file.path(base_dir, folder, sprintf("%s_%d_cropped.tif", var, y))
    stopifnot(file.exists(f)); rast(f)
  })
  list(r = do.call(c, lst), lst = lst)
}
tx <- daymet_stack("tmax", "daymet_cropped")
tn <- daymet_stack("tmin", "daymet_cropped")
pr <- daymet_stack("prcp", "daymet_precip_cropped")
tmax_full <- tx$r; tmin_full <- tn$r; prcp_full <- pr$r

# Daymet years have 365 layers (no Dec 31 in leap years); take dates from the files, else build them
dates_all <- do.call(c, lapply(seq_along(yrs_all), function(i) {
  d <- tryCatch(as.Date(time(tx$lst[[i]])), error = function(e) NA)
  if (length(d) != nlyr(tx$lst[[i]]) || any(is.na(d)))
    d <- as.Date(sprintf("%d-01-01", yrs_all[i])) + 0:(nlyr(tx$lst[[i]]) - 1)
  d
}))
stopifnot(length(dates_all) == nlyr(tmax_full), nlyr(tmin_full) == nlyr(tmax_full),
          nlyr(prcp_full) == nlyr(tmax_full), !any(duplicated(dates_all)))
cat("Climate days:", length(dates_all), "range", format(range(dates_all)), "\n")

# ---------------------------------------------------------------- 2. PHENOLOGY
# Parcel coordinates, projected to the Daymet grid
coords <- read_excel(file.path(base_dir, "parcel_coords.xlsx")) %>% mutate(parcel_id = as.character(parcel_id))
cv <- project(vect(coords, geom = c("lon", "lat"), crs = "EPSG:4326"), crs(tmax_full))
coords_df <- as.data.frame(cv, geom = "XY") %>% mutate(parcel_id = as.character(parcel_id)) %>%
  distinct(parcel_id, .keep_all = TRUE)

# Each year's file is filtered before binding (the raw files are about 13M rows). Kept rows:
# the chosen season, a successful satellite-to-parcel match, and no missing key dates.
plant_all <- bind_rows(lapply(yrs_all, function(y) {
  f <- file.path(plant_dir, sprintf("assigned_year=%d.parquet", y))
  if (!file.exists(f)) { warning("missing ", f); return(NULL) }
  read_parquet(f) %>%
    filter(season == SEASON,
           match_outcome %in% c("matched_adoy_validated", "matched_no_adoy"),
           !is.na(mslsp_OGI), !is.na(mslsp_OGD), !is.na(mslsp_OGMn), !is.na(landiq_PFT)) %>%
    mutate(assigned_year = y, parcel_id = as.character(parcel_id)) %>%
    collect()
}))
stopifnot(nrow(plant_all) > 0)
if (!SUB_COL %in% names(plant_all))
  stop("SUB_COL '", SUB_COL, "' not found. Columns: ", paste(names(plant_all), collapse = ", "))

# One row per parcel-season. key = crop type + LandIQ class letter, e.g. "row_G".
merged <- plant_all %>%
  mutate(OGI_date = as.Date(mslsp_OGI), OGD_date = as.Date(mslsp_OGD), OGMn_date = as.Date(mslsp_OGMn),
         OGI_year = as.integer(format(OGI_date, "%Y")),
         sub = substr(as.character(.data[[SUB_COL]]), 1, 1),
         key = paste(landiq_PFT, sub, sep = "_")) %>%
  left_join(coords_df %>% select(parcel_id, x, y), by = "parcel_id") %>%
  filter(!is.na(x), !is.na(y)) %>%
  as.data.frame()
merged$rid <- seq_len(nrow(merged))        # row id, used to keep tables aligned with the cell index below
cat("merged rows:", nrow(merged), "\n"); print(table(merged$key))
saveRDS(merged %>% distinct(parcel_id, key), file.path(out_dir, "parcel_classes.rds"))

# Parcel -> climate cell. Climate is extracted once per cell, not per parcel.
cell_id <- cellFromXY(tmax_full[[1]], cbind(merged$x, merged$y))
ucell   <- sort(unique(na.omit(cell_id)))
parcel_cell_row <- match(cell_id, ucell)           # row of the cell matrices for each merged row
cell_xy <- xyFromCell(tmax_full[[1]], ucell)
cat("unique climate cells:", length(ucell), "\n")

TX <- as.matrix(terra::extract(tmax_full, ucell))  # cells x days
TN <- as.matrix(terra::extract(tmin_full, ucell))
PR <- as.matrix(terra::extract(prcp_full, cell_xy))
if (ncol(PR) == nlyr(prcp_full) + 1) PR <- PR[, -1]   # drop the ID column that extract() adds for coordinates
stopifnot(ncol(TX) == length(dates_all), ncol(PR) == length(dates_all))
lat_cell <- crds(project(vect(cell_xy, crs = crs(tmax_full)), "EPSG:4326"))[, 2]

# ---------------------------------------------------------------- 3. DAILY SCORE
S_hist <- build_S(TX, TN, PR, dates_all, lat_cell)

# ---------------------------------------------------------------- 4. ANCHORS
onset_matrix <- function(mode) {
  out <- matrix(as.Date(NA), nrow(TN), length(yrs_all))
  for (r in seq_len(nrow(TN))) for (k in seq_along(yrs_all))
    out[r, k] <- find_onset(dates_all, TN[r, ], PR[r, ], yrs_all[k], mode)
  matrix(as.numeric(out), nrow(out))               # dates stored as numbers (days since 1970-01-01)
}
cat("Computing anchors (c7)...\n");  onset_c7  <- onset_matrix("c7")
cat("Computing anchors (rel)...\n"); onset_rel <- onset_matrix("rel")

anchor_diag <- data.frame(year = yrs_all,
  no_anchor_c7  = colMeans(is.na(onset_c7)), no_anchor_rel = colMeans(is.na(onset_rel)))
print(anchor_diag)     # 2018 should be 100% NA (truncated window); it is excluded by design
saveRDS(anchor_diag, file.path(out_dir, "anchor_diagnostics.rds"))

# ---------------------------------------------------------------- 5. OBSERVED TABLES
# days_t = days from the anchor to the observed planting (OGI) or harvest (OGMn) date.
# Rows with implausible durations are dropped. Harvest also needs a plausible OGD -> OGMn span.
make_tables <- function(onset_mat) {
  onset_all <- as.Date(onset_mat[cbind(parcel_cell_row, match(merged$OGI_year, yrs_all))], origin = "1970-01-01")
  base <- merged %>% mutate(onset = onset_all)
  list(ogi  = base %>% mutate(days_t = as.numeric(OGI_date - onset)) %>%
                filter(!is.na(onset), days_t > 0, days_t < 300),
       ogmn = base %>% mutate(d_ogd = as.numeric(OGMn_date - OGD_date),
                              days_t = as.numeric(OGMn_date - onset)) %>%
                filter(d_ogd > 0, d_ogd < 200, !is.na(onset), days_t > 0, days_t < 400))
}
tabs <- list(c7 = make_tables(onset_c7), rel = make_tables(onset_rel))
# Alignment check: the climate cell used must be the cell of THIS parcel
chk <- tabs$c7$ogi
stopifnot(all(cellFromXY(tmax_full[[1]], cbind(chk$x, chk$y)) == ucell[parcel_cell_row[chk$rid]]))
cat("Alignment check passed. Rows:", paste(unlist(lapply(tabs, function(a) lapply(a, nrow))), collapse = " / "), "\n")

tbl_idx <- function(tbl) list(cr = parcel_cell_row[tbl$rid], st = match(tbl$onset, dates_all))

# ---------------------------------------------------------------- 6. VALIDATION
# For each class, calibrate Fcrit on the training years and predict the test year.
# Baseline ("climatology") = mean observed days_t of the training years.
# Parcels whose score never reaches Fcrit fall back to the baseline (they are counted, not dropped).
# A year is a win when the model's RMSE is below the baseline's.
#   scheme "loo"    : leave one year out (train on the other validation years)
#   scheme "forward": train only on EARLIER years (tests prediction forward in time)
run_validation <- function(tbl, win_tbl, anchor, segment, scheme, test_years) {
  res <- list()
  for (key in win_tbl$key) {
    md <- win_tbl$max_days[win_tbl$key == key]
    d  <- tbl %>% filter(key == !!key, OGI_year %in% val_years)
    if (nrow(d) < 50) next
    ix <- tbl_idx(d)
    for (h in test_years) {
      te <- which(d$OGI_year == h)
      tr <- if (scheme == "loo") which(d$OGI_year != h) else which(d$OGI_year < h)
      if (length(te) < 20 || length(tr) < 50) next
      cal <- calib_F(S_hist, ix$cr[tr], ix$st[tr], d$days_t[tr], md)
      k   <- pred_k(S_hist, ix$cr[te], ix$st[te], cal["Fcrit"], md)
      obs <- d$days_t[te]; mu <- mean(d$days_t[tr])
      k_fill <- ifelse(is.na(k), mu, k)
      res[[length(res) + 1]] <- data.frame(anchor, segment, key, year = h, n = length(te),
        Fcrit = unname(cal["Fcrit"]), frac_cross = mean(!is.na(k)),
        rmse_model = sqrt(mean((k_fill - obs)^2)), rmse_null = sqrt(mean((obs - mu)^2)))
    }
  }
  bind_rows(res)
}
run_all <- function(scheme, test_years) {
  bind_rows(
    run_validation(tabs$c7$ogi,   win_ogi,  "c7",  "planting", scheme, test_years),
    run_validation(tabs$c7$ogmn,  win_ogmn, "c7",  "harvest",  scheme, test_years),
    run_validation(tabs$rel$ogi,  win_ogi,  "rel", "planting", scheme, test_years),
    run_validation(tabs$rel$ogmn, win_ogmn, "rel", "harvest",  scheme, test_years)) %>%
    mutate(win = !is.na(rmse_model) & rmse_model < rmse_null)
}
summarise_validation <- function(v) {
  v %>% group_by(anchor, segment, key) %>%
    summarise(wins = sum(win), n_years = n(), mean_frac_cross = mean(frac_cross),
              mean_rmse_model = mean(rmse_model), mean_rmse_null = mean(rmse_null), .groups = "drop") %>%
    mutate(skill_pct = 100 * (1 - mean_rmse_model / mean_rmse_null))   # % lower error than the baseline
}

cat("Leave-one-year-out validation (slow)...\n")
val <- run_all("loo", val_years)
val_summary <- summarise_validation(val)
print(as.data.frame(val_summary))
saveRDS(val, file.path(out_dir, "validation_by_year.rds"))
saveRDS(val_summary, file.path(out_dir, "validation_summary.rds"))

# ---------------------------------------------------------------- 7. FINAL CALIBRATION
# Fcrit per class x anchor x segment, fitted on all validation years. This is what the projections use.
calibrate_all <- function() {
  res <- list()
  for (a in c("c7", "rel")) for (sg in c("planting", "harvest")) {
    tbl <- if (sg == "planting") tabs[[a]]$ogi else tabs[[a]]$ogmn
    wt  <- if (sg == "planting") win_ogi else win_ogmn
    for (key in wt$key) {
      d <- tbl %>% filter(key == !!key, OGI_year %in% val_years); if (nrow(d) < 50) next
      ix <- tbl_idx(d); md <- wt$max_days[wt$key == key]
      cal <- calib_F(S_hist, ix$cr, ix$st, d$days_t, md)
      res[[length(res) + 1]] <- data.frame(anchor = a, segment = sg, key, max_days = md,
        Fcrit = unname(cal["Fcrit"]), rmse_fit = unname(cal["rmse"]), n = nrow(d), mean_days_t = mean(d$days_t))
    }
  }
  bind_rows(res)
}
calib_tab <- calibrate_all()
print(as.data.frame(calib_tab))
saveRDS(calib_tab, file.path(out_dir, "calibrated_Fcrit.rds"))

# ---------------------------------------------------------------- 8. FORWARD-IN-TIME VALIDATION
# Test 2021 (trained on 2019-2020), 2022 (2019-2021) and 2023 (2019-2022).
cat("Forward-in-time validation...\n")
val_fwd <- run_all("forward", fwd_test_years)
fwd_summary <- summarise_validation(val_fwd)
print(as.data.frame(fwd_summary))
saveRDS(val_fwd, file.path(out_dir, "validation_forward_by_year.rds"))
saveRDS(fwd_summary, file.path(out_dir, "validation_forward_summary.rds"))

# ---------------------------------------------------------------- 9. VALIDATED CLASSES
# A class x anchor x segment pair is "validated" if it beats the baseline in enough years in BOTH tests.
# "strong" is the stricter version used for the poster (see 00_setup.R).
pairs_tab <- val_summary %>%
  select(anchor, segment, key, loo_wins = wins, loo_error_model = mean_rmse_model,
         loo_error_baseline = mean_rmse_null, loo_skill_pct = skill_pct) %>%
  left_join(fwd_summary %>% select(anchor, segment, key, fwd_wins = wins, fwd_skill_pct = skill_pct),
            by = c("anchor", "segment", "key")) %>%
  mutate(fwd_wins  = coalesce(fwd_wins, 0L),
         validated = loo_wins >= LOO_MIN_WINS_VALIDATED & fwd_wins >= FWD_MIN_WINS,
         strong    = loo_wins >= LOO_MIN_WINS_STRONG    & fwd_wins >= FWD_MIN_WINS)
saveRDS(pairs_tab, file.path(out_dir, "validated_pairs.rds"))
cat("\nPairs tested:", nrow(pairs_tab), "| validated:", sum(pairs_tab$validated), "| strong:", sum(pairs_tab$strong), "\n")
print(as.data.frame(pairs_tab %>% filter(validated)))
cat("\nDONE. Outputs in", out_dir, "\n")
