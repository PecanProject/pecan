#!/usr/bin/env Rscript
# design_points.csv (UniqueID, site_id, lat, lon) × 2018–2023 joined to LandIQ v4.1
# crops_all_years.parq on (UniqueID, year). Long format: one row per site × parcel × year ×
# season that exists in LandIQ (inner join — no placeholder rows for missing years).
# Sorted by parcel_id, year, season. No geometry.
# Output: Parquet + CSV under CCMMF_MANAGEMENT (repo root by default).
# Columns: site_id, parcel_id, lon, lat, year, season, CLASS, SUBCLASS, CLASS_desc,
#   SUBCLASS_desc, PFT
# PFT comes from LandIQ_cropCode_lookup_table.csv (merge on CLASS + SUBCLASS). Only rows with
# is_agricultural == TRUE in that table are kept (same rule as trait / phenology joins).
# Missing/blank SUBCLASS is coerced to "**" before the merge. The lookup includes explicit
# (CLASS, **) rows for each agricultural class except T (modal PFT per class: row/rice/hay/woody/etc.).
# If merge still leaves PFT NA (e.g. CLASS missing in LandIQ, or unmapped pair), impute PFT to
# "woody" when subclass was missing and CLASS is not T. CLASS T with missing subclass keeps PFT NA.
# (Optional: flag_TRUCK_no_subclass — see commented block below.)
# site_id only ever comes from design_points; NA means a blank/missing site_id in the CSV.
# The same UniqueID can appear on multiple design rows (different site_id) if the file duplicates CARB ids.
# LandIQ stores UniqueID as character (often with leading zeros). Read UniqueID as character — do not use
# data.table::fread() on design_points.csv (it coerces numeric-looking IDs to integers and drops leading zeros).
# Panel: by default only design UniqueIDs with ≥1 agricultural LandIQ row are kept (see `dp_panel` below).
# Comment/uncomment there to include design sites that have no ag LandIQ in 2018–2023.

library(data.table)
library(readr)
library(dplyr)
library(arrow)

path_design <- Sys.getenv("DESIGN_POINTS_CSV", "/projectnb/dietzelab/XinyuanJi/design_points.csv")
path_landiq <- Sys.getenv("CCMMF_LANDIQ_V4", "/projectnb/dietzelab/ccmmf/LandIQ-harmonized-v4.1")
path_management <- Sys.getenv("CCMMF_MANAGEMENT", "/projectnb/dietzelab/ccmmf/management")
path_parq <- file.path(path_landiq, "crops_all_years.parq")
path_lookup <- file.path(path_management, "LandIQ_cropCode_lookup_table.csv")
path_out <- Sys.getenv(
  "OUT_PARQUET",
  file.path(path_management, "design_points_landiq_2018-2023.parquet")
)

years <- 2018L:2023L

dp <- as.data.table(
  read_csv(
    path_design,
    show_col_types = FALSE,
    col_types = cols(UniqueID = col_character())
  )
)
dp[, UniqueID := trimws(as.character(UniqueID))]

uids <- unique(dp$UniqueID)

crops <- as.data.table(
  arrow::open_dataset(path_parq) |>
    dplyr::filter(year %in% !!years, UniqueID %in% !!uids) |>
    dplyr::collect()
)
crops[, UniqueID := as.character(UniqueID)]
crops[, parcel_id := as.character(parcel_id)]
crops[, year := as.integer(year)]
# SUBCLASS: trimws() aligns padded LandIQ strings with lookup keys; needed for missing-subclass rules.
crops[, SUBCLASS := trimws(as.character(SUBCLASS))]
crops[, subclass_missing_src := is.na(SUBCLASS) | !nzchar(SUBCLASS) | SUBCLASS == "**"]
crops[is.na(SUBCLASS) | SUBCLASS == "" | SUBCLASS == "**", SUBCLASS := "**"]
crops[, CLASS := as.character(CLASS)]

lookup <- fread(path_lookup)
lookup[, CLASS := as.character(CLASS)]
lookup[, SUBCLASS := as.character(SUBCLASS)]

crops <- merge(
  crops,
  unique(lookup[, .(CLASS, SUBCLASS, CLASS_desc, SUBCLASS_desc, PFT, is_agricultural)]),
  by = c("CLASS", "SUBCLASS"),
  all.x = TRUE
)
crops[, is_agricultural := tolower(trimws(as.character(is_agricultural))) == "true"]
crops <- crops[is_agricultural == TRUE]
crops[, is_agricultural := NULL]
# # Truck (T) with missing subclass: do not guess PFT — flag for manual review (lookup has no T,**).
# # Uncomment when you need `flag_TRUCK_no_subclass` in the output (also add it to `landiq_long` and `out` below).
# crops[, flag_TRUCK_no_subclass := CLASS == "T" & subclass_missing_src]
# Other CLASS with missing subclass and no lookup match: default PFT to woody.
crops[
  is.na(PFT) & subclass_missing_src & CLASS != "T" & !is.na(CLASS) & nzchar(CLASS),
  PFT := "woody"
]

landiq_long <- crops[, .(
  UniqueID, parcel_id, year, season, CLASS, SUBCLASS, PFT, CLASS_desc, SUBCLASS_desc
)]

uids_with_liq <- unique(landiq_long$UniqueID)
# Keep only design locations that have ≥1 ag LandIQ row (drops sites with no joined data).
dp_panel <- dp[UniqueID %in% uids_with_liq]
# dp_panel <- dp # include all design sites (panel rows only where `out` merge finds LandIQ)

sm <- unique(dp_panel[, .(site_id, UniqueID, lon, lat)])
n_sm <- nrow(sm)
panel <- sm[rep(seq_len(n_sm), each = length(years))]
panel[, year := rep(years, times = n_sm)]

out <- merge(
  panel,
  landiq_long,
  by = c("UniqueID", "year"),
  sort = FALSE
)

out[, UniqueID := NULL]

out <- out[, .(
  site_id, parcel_id, lon, lat, year,
  season, CLASS, SUBCLASS, CLASS_desc, SUBCLASS_desc, PFT
)]
setorder(out, parcel_id, year, season)

dir.create(dirname(path_out), recursive = TRUE)
arrow::write_parquet(out, path_out)
path_csv <- file.path(dirname(path_out), paste0(tools::file_path_sans_ext(basename(path_out)), ".csv"))
fwrite(out, path_csv)
message("Wrote: ", path_out, " (", nrow(out), " rows)")
message("Wrote: ", path_csv, " (", nrow(out), " rows)")
