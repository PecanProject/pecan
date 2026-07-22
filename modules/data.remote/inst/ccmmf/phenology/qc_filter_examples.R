# =============================================================================
# QC filter examples for MSLSP assignment output
# =============================================================================
# Copy-paste the blocks you need. Adjust out_dir and year as needed.
# Filter assigned_by == "matched" for event-ready rows.
# =============================================================================

library(data.table)
library(arrow)

path_management <- Sys.getenv("CCMMF_MANAGEMENT", "/projectnb/dietzelab/ccmmf/management")
out_dir         <- file.path(path_management, "phenology/matched_landiq_mslsp_v4.1.2")
year            <- 2023
assigned        <- as.data.table(arrow::read_parquet(file.path(out_dir, paste0("assigned_year=", year, ".parquet"))))
matched         <- assigned[assigned_by == "matched"]

# -----------------------------------------------------------------------------
# 1. Filter for analysis-ready pairs (ADOY validated)
#    Field ADOY falls inside the MSLSP cycle window (OGI–OGMn).
# -----------------------------------------------------------------------------
# analysis_ready <- matched[qc_adoy_vs_cycle == "adoy_inside_cycle"]

# -----------------------------------------------------------------------------
# 2. Exclude high-masking parcels
#    Keep parcels with low fraction of masked (cloud/shadow) pixels.
# -----------------------------------------------------------------------------
# low_na <- matched[qc_heterogeneity == "low_na_frac"]

# -----------------------------------------------------------------------------
# 3. Combined: ADOY validated + low masking
# -----------------------------------------------------------------------------
# analysis_ready_low_na <- matched[qc_adoy_vs_cycle == "adoy_inside_cycle" &
#                                 qc_heterogeneity == "low_na_frac"]

# -----------------------------------------------------------------------------
# 4. Review ADOY-outside cases
#    Field ADOY is outside the cycle window; worth manual review.
# -----------------------------------------------------------------------------
# adoy_outside <- matched[qc_adoy_vs_cycle == "adoy_outside_cycle"]

# -----------------------------------------------------------------------------
# 5. Parcels with no valid MSLSP cycles
#    MSLSP had data but all cycles were filtered out (e.g. zero valid pixels).
# -----------------------------------------------------------------------------
# no_mslsp <- assigned[match_outcome == "mslsp_cycles_filtered_out"][
#   , .(parcel_id, year)] |> unique()

# -----------------------------------------------------------------------------
# 6. Parcels with cycle/season mismatch
#    2 MSLSP cycles but 1 LandIQ season, or 1 cycle but 2 seasons.
# -----------------------------------------------------------------------------
# mismatch_2c1s <- assigned[match_outcome == "mismatch_2cycles_1season"][
#   , .(parcel_id, year)] |> unique()
# mismatch_1c2s <- assigned[match_outcome == "mismatch_1cycle_2seasons"][
#   , .(parcel_id, year)] |> unique()

# -----------------------------------------------------------------------------
# 7. Field-year outcome breakdown (match_outcome counts)
# -----------------------------------------------------------------------------
# assigned[, .(match_outcome = match_outcome[!is.na(match_outcome)][1L]),
#          by = .(parcel_id, year)][, .N, by = match_outcome][order(-N)]
