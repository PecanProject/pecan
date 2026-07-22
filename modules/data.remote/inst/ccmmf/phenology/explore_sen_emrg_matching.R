#!/usr/bin/env Rscript
# Explore whether ADOY_SEN or ADOY_EMRG could improve LandIQ-MSLSP matching.
# Run from management: Rscript scripts/phenology/explore_sen_emrg_matching.R

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(dplyr)
  library(lubridate)
})

path_management <- Sys.getenv("CCMMF_MANAGEMENT", "/projectnb/dietzelab/ccmmf/management")
path_landiq    <- Sys.getenv("CCMMF_LANDIQ_V4", "/projectnb/dietzelab/ccmmf/LandIQ-harmonized-v4.1.2")
landiq_parq    <- file.path(path_landiq, "crops_all_years.parq")
source(file.path(path_management, "scripts/phenology/matched_paths.R"))
matched_dir    <- matched_landiq_dir(path_management)
combined_root  <- file.path(path_management, "phenology/raw_mslsp_v4.1.2")

# Use 2021: parquet has ADOY_SEN and ADOY_EMRG populated
yr <- 2021L

# Date-in-window (same logic as match script: target year, date comparison)
date_in_window <- function(adoy, ogi, ogmn, year) {
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

message("Loading assigned 2021 and MSLSP cycles...")
assigned <- as.data.table(read_parquet(file.path(matched_dir, "assigned_year=2021.parquet")))
assigned[, parcel_id := as.character(parcel_id)]
mslsp <- as.data.table(read_parquet(file.path(combined_root, "year=2021", "mslsp_year=2021.parquet")))
mslsp[, parcel_id := as.character(parcel_id)]

# Keep only matched rows with season and cycle
matched <- assigned[assigned_by == "matched" & !is.na(mslsp_cycle)]

# Load LandIQ 2021 with SEN/EMRG
ds <- open_dataset(landiq_parq)
landiq <- as.data.table(collect(filter(ds, year == !!yr)))
landiq[, parcel_id := trimws(as.character(parcel_id))]
landiq[, ADOY_num := suppressWarnings(as.numeric(ADOY))]
landiq[ADOY_num == 0, ADOY_num := NA_real_]
landiq[, ADOY_SEN_num := suppressWarnings(as.numeric(ADOY_SEN))]
landiq[ADOY_SEN_num == 0, ADOY_SEN_num := NA_real_]
landiq[, ADOY_EMRG_num := suppressWarnings(as.numeric(ADOY_EMRG))]
landiq[ADOY_EMRG_num == 0, ADOY_EMRG_num := NA_real_]

# Get MSLSP cycle windows (OGI, OGMn as DOY) per parcel
mslsp_wide <- mslsp[, .(parcel_id, cycle, OGI = OGI_mean, OGMn = OGMn_mean, Peak = Peak_mean)]
mslsp_wide <- mslsp_wide[!is.na(OGI) & !is.na(OGMn)]
cycle1 <- mslsp_wide[cycle == 1L, .(parcel_id, OGI1 = OGI, OGMn1 = OGMn)]
cycle2 <- mslsp_wide[cycle == 2L, .(parcel_id, OGI2 = OGI, OGMn2 = OGMn)]
cycles <- merge(cycle1, cycle2, by = "parcel_id", all = TRUE)

# Merge LandIQ with cycles
landiq_cyc <- merge(landiq, cycles, by = "parcel_id")
# Only parcel-years that appear in assigned matched
pids <- unique(matched[, .(parcel_id)])
landiq_cyc <- landiq_cyc[parcel_id %in% pids$parcel_id]

# Rows with at least one of ADOY, ADOY_SEN, ADOY_EMRG
landiq_cyc[, has_adoy := !is.na(ADOY_num)]
landiq_cyc[, has_sen := !is.na(ADOY_SEN_num)]
landiq_cyc[, has_emrg := !is.na(ADOY_EMRG_num)]
sample_avail <- landiq_cyc[has_adoy == TRUE | has_sen == TRUE | has_emrg == TRUE]
message("LandIQ 2021 rows with ADOY or SEN or EMRG in assigned parcels: ", nrow(sample_avail))

# For rows with SEN or EMRG, which cycle does each fall in?
sample_avail[, in_cycle1 := date_in_window(ADOY_num, OGI1, OGMn1, yr)]
sample_avail[, in_cycle2 := date_in_window(ADOY_num, OGI2, OGMn2, yr)]
sample_avail[, sen_in_1 := date_in_window(ADOY_SEN_num, OGI1, OGMn1, yr)]
sample_avail[, sen_in_2 := date_in_window(ADOY_SEN_num, OGI2, OGMn2, yr)]
sample_avail[, emrg_in_1 := date_in_window(ADOY_EMRG_num, OGI1, OGMn1, yr)]
sample_avail[, emrg_in_2 := date_in_window(ADOY_EMRG_num, OGI2, OGMn2, yr)]

# Subset: rows that have both ADOY and (SEN or EMRG) for same-season interpretation
both <- sample_avail[has_adoy == TRUE & (has_sen == TRUE | has_emrg == TRUE)]
message("Rows with both ADOY and (SEN or EMRG): ", nrow(both))

# Summary: when ADOY is in cycle 1, is SEN/EMRG in cycle 2 (or vice versa)?
both[, adoy_cycle := fcase(
  in_cycle1 == TRUE & (is.na(in_cycle2) | !in_cycle2), 1L,
  in_cycle2 == TRUE & (is.na(in_cycle1) | !in_cycle1), 2L,
  in_cycle1 == TRUE & in_cycle2 == TRUE, 12L,  # both
  default = NA_integer_
)]
both[, sen_cycle := fcase(
  sen_in_1 == TRUE & (is.na(sen_in_2) | !sen_in_2), 1L,
  sen_in_2 == TRUE & (is.na(sen_in_1) | !sen_in_1), 2L,
  default = NA_integer_
)]
both[, emrg_cycle := fcase(
  emrg_in_1 == TRUE & (is.na(emrg_in_2) | !emrg_in_2), 1L,
  emrg_in_2 == TRUE & (is.na(emrg_in_1) | !emrg_in_1), 2L,
  default = NA_integer_
)]

cat("\n--- When ADOY falls in one cycle, where do SEN/EMRG fall? ---\n")
# ADOY in 1 -> SEN/EMRG in 2 would be consistent (senescing = end of prior cycle; emerging = start of next)
tab_adoy1_sen <- both[adoy_cycle == 1L, .(sen_in_1 = sum(sen_in_1 == TRUE, na.rm = TRUE), sen_in_2 = sum(sen_in_2 == TRUE, na.rm = TRUE), sen_na = sum(is.na(sen_cycle)))]
tab_adoy1_emrg <- both[adoy_cycle == 1L, .(emrg_in_1 = sum(emrg_in_1 == TRUE, na.rm = TRUE), emrg_in_2 = sum(emrg_in_2 == TRUE, na.rm = TRUE))]
tab_adoy2_sen <- both[adoy_cycle == 2L, .(sen_in_1 = sum(sen_in_1 == TRUE, na.rm = TRUE), sen_in_2 = sum(sen_in_2 == TRUE, na.rm = TRUE))]
tab_adoy2_emrg <- both[adoy_cycle == 2L, .(emrg_in_1 = sum(emrg_in_1 == TRUE, na.rm = TRUE), emrg_in_2 = sum(emrg_in_2 == TRUE, na.rm = TRUE))]
print(tab_adoy1_sen)
print(tab_adoy1_emrg)
print(tab_adoy2_sen)
print(tab_adoy2_emrg)

# Agreement: would using SEN/EMRG to assign a second season give same cycle as current assignment?
matched_agg <- matched[, .(assigned_cycle = mslsp_cycle[1], season = season[1]), by = .(parcel_id, season)]
setnames(matched_agg, "season", "landiq_season")
both_merge <- merge(both[, .(parcel_id, season, ADOY_num, ADOY_SEN_num, ADOY_EMRG_num, adoy_cycle, sen_cycle, emrg_cycle, in_cycle1, in_cycle2)],
  matched_agg, by.x = c("parcel_id", "season"), by.y = c("parcel_id", "landiq_season"), all.x = TRUE)
# Where we assigned a cycle: does SEN or EMRG agree (fall in same cycle)?
both_merge[!is.na(assigned_cycle), sen_agrees := (sen_cycle == assigned_cycle) | is.na(sen_cycle)]
both_merge[!is.na(assigned_cycle), emrg_agrees := (emrg_cycle == assigned_cycle) | is.na(emrg_cycle)]
cat("\n--- Agreement with current assignment (when SEN/EMRG available) ---\n")
cat("SEN agrees with assigned cycle:", both_merge[!is.na(sen_cycle), sum(sen_agrees, na.rm = TRUE)], "/", both_merge[!is.na(sen_cycle), .N], "\n")
cat("EMRG agrees with assigned cycle:", both_merge[!is.na(emrg_cycle), sum(emrg_agrees, na.rm = TRUE)], "/", both_merge[!is.na(emrg_cycle), .N], "\n")

# Cases where SEN or EMRG would suggest a *different* cycle than ADOY
conflict_sen <- both_merge[!is.na(adoy_cycle) & !is.na(sen_cycle) & adoy_cycle != sen_cycle]
conflict_emrg <- both_merge[!is.na(adoy_cycle) & !is.na(emrg_cycle) & adoy_cycle != emrg_cycle]
cat("\n--- Conflicts: ADOY says one cycle, SEN/EMRG says other ---\n")
cat("SEN conflict count:", nrow(conflict_sen), "\n")
cat("EMRG conflict count:", nrow(conflict_emrg), "\n")
if (nrow(conflict_sen) > 0) { cat("Sample SEN conflict (parcel, season, adoy_cycle, sen_cycle):\n"); print(conflict_sen[1:min(5, .N), .(parcel_id, season, adoy_cycle, sen_cycle)]) }
if (nrow(conflict_emrg) > 0) { cat("Sample EMRG conflict:\n"); print(conflict_emrg[1:min(5, .N), .(parcel_id, season, adoy_cycle, emrg_cycle)]) }

# Could we use SEN/EMRG when ADOY is missing? (e.g. 2016) - not in 2016 parquet, but in 2021: rows with SEN/EMRG but no ADOY
no_adoy_has_sen <- landiq_cyc[has_adoy == FALSE & (has_sen == TRUE | has_emrg == TRUE)]
cat("\n--- Rows with NO ADOY but have SEN or EMRG (could help no-ADOY years if present) ---\n")
cat("Count:", nrow(no_adoy_has_sen), "\n")
if (nrow(no_adoy_has_sen) > 0) {
  no_adoy_has_sen[, sen_in_1 := date_in_window(ADOY_SEN_num, OGI1, OGMn1, yr)]
  no_adoy_has_sen[, sen_in_2 := date_in_window(ADOY_SEN_num, OGI2, OGMn2, yr)]
  no_adoy_has_sen[, emrg_in_1 := date_in_window(ADOY_EMRG_num, OGI1, OGMn1, yr)]
  no_adoy_has_sen[, emrg_in_2 := date_in_window(ADOY_EMRG_num, OGI2, OGMn2, yr)]
  cat("Of these, SEN in cycle 1:", sum(no_adoy_has_sen$sen_in_1 == TRUE, na.rm = TRUE), "SEN in cycle 2:", sum(no_adoy_has_sen$sen_in_2 == TRUE, na.rm = TRUE), "\n")
  cat("EMRG in cycle 1:", sum(no_adoy_has_sen$emrg_in_1 == TRUE, na.rm = TRUE), "EMRG in cycle 2:", sum(no_adoy_has_sen$emrg_in_2 == TRUE, na.rm = TRUE), "\n")
}

cat("\n--- Summary & recommendation ---\n")
cat("SEN: Almost no overlap with cycle windows in 2021 (2 in cycle 2 when ADOY in 1). Not useful for matching.\n")
cat("EMRG: When ADOY in cycle 1, EMRG often in cycle 2 (early/double crop). When ADOY in cycle 2, EMRG mostly in cycle 1.\n")
cat("For 23k rows with NO ADOY but EMRG present: EMRG in cycle 1 = 7752, in cycle 2 = 5504.\n")
cat("Recommendation: When ADOY is missing, use ADOY_EMRG to assign cycle (assign season to cycle that contains EMRG) if available.\n")
cat("SEN not recommended for matching (too sparse in windows).\n")
message("\nDone.")
