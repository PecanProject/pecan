#!/usr/bin/env Rscript
# LandIQ harmonized v4: crop × season × year counts and probabilities.
#
# Outputs (under out_dir):
#   - crop_season_year_counts.csv       — long counts: year, season, crop, N
#   - crop_season_year_p_long.csv       — P(crop | year, season); sums to 1 within each (year, season)
#   - crop_season_year_p_wide_s*.csv    — one wide table per season: years × crops (probabilities)
#
# Crop label: paste(CLASS, SUBCLASS) by default (use crop_key = "CLASS" for CLASS only).
#
# Usage:
#   module load R/4.4.3   # or your R module with data.table, arrow, dplyr
#   Rscript landiq_crop_season_year_probabilities.R
#   YEAR_MIN=2016 YEAR_MAX=2023 Rscript landiq_crop_season_year_probabilities.R

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(dplyr)
})

path_management <- Sys.getenv("CCMMF_MANAGEMENT", "/projectnb/dietzelab/ccmmf/management")
path_landiq_v4  <- Sys.getenv("CCMMF_LANDIQ_V4", "/projectnb/dietzelab/ccmmf/LandIQ-harmonized-v4.1")
landiq_parq     <- file.path(path_landiq_v4, "crops_all_years.parq")
lookup_csv      <- file.path(path_management, "LandIQ_cropCode_lookup_table.csv")
out_dir         <- Sys.getenv(
  "LANDIQ_PROB_OUT",
  file.path(path_management, "phenology", "landiq_crop_season_year_probs")
)

year_min <- as.integer(Sys.getenv("YEAR_MIN", "2018"))
year_max <- as.integer(Sys.getenv("YEAR_MAX", "2023"))
ag_only  <- tolower(Sys.getenv("AG_ONLY", "true")) %in% c("1", "true", "yes")
crop_key <- tolower(Sys.getenv("CROP_KEY", "class_subclass")) # or "class"
dedupe   <- tolower(Sys.getenv("DEDUPE_PARCEL_YEAR_SEASON", "false")) %in% c("1", "true", "yes")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

lookup <- fread(lookup_csv)
ag_classes <- unique(lookup[is_agricultural == TRUE, as.character(CLASS)])

ds <- open_dataset(landiq_parq)

# Note: is_in(CLASS, Array$create(...)) errors on some Arrow builds; %in% works.
sel <- ds |>
  dplyr::filter(year >= !!year_min, year <= !!year_max) |>
  dplyr::select(parcel_id, year, season, CLASS, SUBCLASS)

if (ag_only) {
  sel <- sel |> dplyr::filter(CLASS %in% ag_classes)
}

dt <- as.data.table(collect(sel))
dt[, `:=`(
  parcel_id = as.character(parcel_id),
  year = as.integer(year),
  season = as.integer(season),
  CLASS = trimws(as.character(CLASS)),
  SUBCLASS = as.character(SUBCLASS)
)]

if (dedupe) {
  dt <- unique(dt, by = c("parcel_id", "year", "season"))
}

if (crop_key == "class") {
  dt[, crop := CLASS]
} else {
  dt[, crop := fifelse(
    is.na(SUBCLASS) | SUBCLASS == "",
    CLASS,
    paste(CLASS, SUBCLASS, sep = "_")
  )]
}

dt <- dt[!is.na(crop) & nzchar(crop) & crop != "**"]

counts <- dt[, .N, by = .(year, season, crop)]
setorder(counts, year, season, -N)

counts[, p_given_year_season := N / sum(N), by = .(year, season)]

fwrite(counts[, .(year, season, crop, N)], file.path(out_dir, "crop_season_year_counts.csv"))
fwrite(
  counts[, .(year, season, crop, N, p_given_year_season)],
  file.path(out_dir, "crop_season_year_p_long.csv")
)

for (s in sort(unique(counts$season))) {
  wide <- dcast(
    counts[season == s],
    year ~ crop,
    value.var = "p_given_year_season",
    fill = 0
  )
  fwrite(wide, file.path(out_dir, sprintf("crop_season_year_p_wide_s%d.csv", s)))
}

# Optional: P(crop | season) pooling all years (marginal over year, equal weight per record)
pool <- dt[, .N, by = .(season, crop)]
pool[, p_given_season := N / sum(N), by = season]
setorder(pool, season, -N)
fwrite(pool, file.path(out_dir, "crop_season_pooled_by_season.csv"))

message("Wrote tables to ", out_dir)
message("Rows used: ", nrow(dt), "  (dedupe_parcel_year_season=", dedupe, ", ag_only=", ag_only, ")")
