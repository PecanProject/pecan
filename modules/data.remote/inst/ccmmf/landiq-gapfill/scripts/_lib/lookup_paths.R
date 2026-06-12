# Resolve the training-year suffix used in the CDL x LandIQ subclass lookup
# filenames. build_emission.R (writer) and prep.R (reader) must derive the same
# suffix from the same env vars so the lookup files round-trip without a sidecar
# metadata CSV.
#
# Default emission years: landiq_emission_training_years() in gapfill_config.R
# (LandIQ parquet years with matching CDL fraction parquets, minus exclusions).
#
# Manual overrides (optional):
#   CDL_LANDIQ_TRAINING_YEARS            comma list
#   CDL_LANDIQ_TRAINING_YEAR_MIN/MAX     inclusive range
#   CDL_LANDIQ_TRAINING_EXCLUDE_YEARS    default 2017
#
# Filename suffix examples:
#   2016-2024 excluding 2017     -> "2016-2024_excl2017"
#   2016-2023 (no exclusions)    -> "2016-2023"

landiq_lookup_years <- function() {
  train_years <- landiq_emission_training_years()
  yr_min <- min(train_years)
  yr_max <- max(train_years)
  excluded <- sort(setdiff(seq.int(yr_min, yr_max), train_years))
  list(
    yr_min = yr_min,
    yr_max = yr_max,
    excluded = excluded,
    train_years = train_years
  )
}

landiq_lookup_suffix <- function() {
  y <- landiq_lookup_years()
  suf <- sprintf("%d-%d", y$yr_min, y$yr_max)
  if (length(y$excluded) > 0L) {
    suf <- paste0(suf, "_excl", paste(y$excluded, collapse = "-"))
  }
  suf
}
