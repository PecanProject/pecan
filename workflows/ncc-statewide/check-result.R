#!/usr/bin/env Rscript

config <- config::get(file = "workflows/ncc-statewide/config.yml",
                      config = Sys.getenv("NCC_PROJECT", "default"))

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

# rows per material
print(
  ds |>
    dplyr::count(.data$material) |>
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

# org_c and org_n ranges
stats <- ds |>
  dplyr::summarize(
    min_org_c  = min(.data$org_c_kg_m2, na.rm = TRUE),
    max_org_c  = max(.data$org_c_kg_m2, na.rm = TRUE),
    mean_org_c = mean(.data$org_c_kg_m2, na.rm = TRUE),
    min_org_n  = min(.data$org_n_kg_m2, na.rm = TRUE),
    max_org_n  = max(.data$org_n_kg_m2, na.rm = TRUE),
    mean_org_n = mean(.data$org_n_kg_m2, na.rm = TRUE)
  ) |>
  dplyr::collect()
print(stats |> as.data.frame())

# checks. these assert the properties the workflow is supposed to guarantee, so a
# silently malformed run fails here rather than in SIPNET
dat <- ds |> dplyr::collect()
props <- PEcAn.data.land::ca_organic_amendment_properties

fail <- function(...) PEcAn.logger::logger.severe(...)

if (anyNA(dat$date) || anyNA(dat$org_n_kg_m2) || anyNA(dat$org_c_kg_m2)) {
  fail("NA in date, org_n_kg_m2 or org_c_kg_m2")
}

# amendment N is reported as organic; mineral N is only populated when it is known to
# be present at application, which these materials do not report
if (any(dat$nh4_n_kg_m2 != 0) || any(dat$no3_n_kg_m2 != 0)) {
  fail("mineral N is non zero; amendment N should be carried as organic N")
}

if (any(dat$org_n_kg_m2 <= 0) || any(dat$org_c_kg_m2 <= 0)) {
  fail("non positive org_n_kg_m2 or org_c_kg_m2")
}

# org_c/org_n must reproduce the material C:N, which is what drives decomposition
cn_out <- dat$org_c_kg_m2 / dat$org_n_kg_m2
cn_range <- range(c(props$cn_min, props$cn_max))
if (min(cn_out) < cn_range[1] - 1e-6 || max(cn_out) > cn_range[2] + 1e-6) {
  fail(sprintf("delivered C:N %.1f to %.1f falls outside the material range %.1f to %.1f",
               min(cn_out), max(cn_out), cn_range[1], cn_range[2]))
}

unknown_material <- setdiff(dat$material, props$material)
if (length(unknown_material) > 0) {
  fail("materials not in ca_organic_amendment_properties: ",
       paste(unknown_material, collapse = ", "))
}

# the material draw is uniform over eligible materials, so no material should carry a
# share far from the mean. a wide tolerance still catches a pool indexed the wrong way
share <- table(dat$material) / nrow(dat)
if (max(share) > 3 * mean(share)) {
  fail(sprintf("material selection is skewed: %s holds %.1f%% against a %.1f%% mean",
               names(share)[which.max(share)], 100 * max(share), 100 * mean(share)))
}

n_ens <- as.integer(config[["n_ensemble"]])
if (dplyr::n_distinct(dat$ens_id) != n_ens) {
  fail(sprintf("found %d ensemble members, expected %d",
               dplyr::n_distinct(dat$ens_id), n_ens))
}

# events are anchored up to 210 days before green-up, so a cycle in year Y can place its
# application in Y-1. that includes 2017, which carries no crop cycles of its own
expected_years <- as.integer(config[["years"]])
allowed_years <- seq(min(expected_years) - 1L, max(expected_years))
extra_years <- setdiff(unique(lubridate::year(dat$date)), allowed_years)
if (length(extra_years) > 0) {
  fail("event dates outside the configured years: ",
       paste(sort(extra_years), collapse = ", "))
}

PEcAn.logger::logger.info("All checks passed")
