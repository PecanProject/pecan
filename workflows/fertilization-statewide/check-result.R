#!/usr/bin/env Rscript

config <- config::get(file = "workflows/fertilization-statewide/config.yml",
                      config = Sys.getenv("FERT_PROJECT", "default"))

options(arrow.unsafe_metadata = TRUE)

out_path <- config[["output_dir"]]
if (!dir.exists(out_path)) {
  PEcAn.logger::logger.severe("Output not found: ", out_path)
}

shards <- list.files(out_path, pattern = "\\.parquet$", full.names = TRUE)
if (length(shards) == 0) {
  PEcAn.logger::logger.severe("No parquet shards under: ", out_path)
}
ds <- arrow::open_dataset(shards)

# schema
print(ds$schema)

PEcAn.logger::logger.info(sprintf(
  "%d shards: %s ... %s",
  length(shards), basename(shards[1]), basename(shards[length(shards)])))

tot <- ds |> dplyr::count() |> dplyr::collect()
PEcAn.logger::logger.info(sprintf("total rows: %d", tot[["n"]]))

# rows per ensemble member
print(
  ds |>
    dplyr::count(.data$ens_id) |>
    dplyr::collect() |>
    as.data.frame()
)

# rows per year
print(
  ds |>
    dplyr::mutate(year = lubridate::year(.data$date)) |>
    dplyr::count(.data$year) |>
    dplyr::collect() |>
    dplyr::arrange(.data$year) |>
    as.data.frame()
)

# first 5 rows
print(ds |> dplyr::slice_head(n = 5) |> dplyr::collect() |> as.data.frame())

# nh4 and no3 total range (kg N/m^2)
stats <- ds |>
  dplyr::mutate(total_n = .data$nh4_n_kg_m2 + .data$no3_n_kg_m2) |>
  dplyr::summarize(
    min_total_n  = min(.data$total_n, na.rm = TRUE),
    max_total_n  = max(.data$total_n, na.rm = TRUE),
    mean_total_n = mean(.data$total_n, na.rm = TRUE)
  ) |>
  dplyr::collect()
print(stats |> as.data.frame())

# checks. these assert the properties the workflow is supposed to guarantee, so a
# silently malformed run fails here rather than in SIPNET
dat <- ds |> dplyr::collect()
rates <- PEcAn.data.land::ca_n_application_rate

fail <- function(...) PEcAn.logger::logger.severe(...)

if (anyNA(dat$date) || anyNA(dat$nh4_n_kg_m2) || anyNA(dat$no3_n_kg_m2)) {
  fail("NA in date, nh4_n_kg_m2 or no3_n_kg_m2")
}

# synthetic fertilizer carries no organic fraction
if (any(dat$org_c_kg_m2 != 0) || any(dat$org_n_kg_m2 != 0)) {
  fail("organic C or N is non zero; synthetic N events should carry mineral N only")
}

total_n <- dat$nh4_n_kg_m2 + dat$no3_n_kg_m2
if (any(total_n <= 0)) {
  fail("events with no applied N")
}

# every event must fall inside the published per crop envelope
envelope <- PEcAn.utils::ud_convert(
  c(min(rates$min_n_lbs_acre), max(rates$max_n_lbs_acre)), "lb/acre", "kg/m^2")
if (min(total_n) < envelope[1] - 1e-9 || max(total_n) > envelope[2] + 1e-9) {
  fail(sprintf("applied N %.4g to %.4g kg/m2 falls outside the ca_n_application_rate envelope %.4g to %.4g",
               min(total_n), max(total_n), envelope[1], envelope[2]))
}

# the mineral split is a configured constant, so it should hold on every row
nh4_fraction <- as.numeric(config[["nh4_fraction"]])
observed <- dat$nh4_n_kg_m2 / total_n
if (any(abs(observed - nh4_fraction) > 1e-6)) {
  fail(sprintf("nh4 share departs from nh4_fraction=%.2f (observed %.4f to %.4f)",
               nh4_fraction, min(observed), max(observed)))
}

n_ens <- as.integer(config[["n_ensemble"]])
if (dplyr::n_distinct(dat$ens_id) != n_ens) {
  fail(sprintf("found %d ensemble members, expected %d",
               dplyr::n_distinct(dat$ens_id), n_ens))
}

# the event date is the green-up date itself, and the phenology product assigns
# some green-ups to the previous calendar year, so year Y can carry a Y-1 date
expected_years <- as.integer(config[["years"]])
allowed_years <- seq(min(expected_years) - 1L, max(expected_years))
extra_years <- setdiff(unique(lubridate::year(dat$date)), allowed_years)
if (length(extra_years) > 0) {
  fail("event dates outside the configured years: ",
       paste(sort(extra_years), collapse = ", "))
}

PEcAn.logger::logger.info("All checks passed")
