#!/usr/bin/env Rscript

config <- config::get(file = "workflows/ncc-statewide/config.yml",
                      config = Sys.getenv("NCC_PROJECT", "default"))

# an override set to an empty string is treated as unset
ccmmf_dir <- Sys.getenv("CCMMF_DIR")
if (!nzchar(ccmmf_dir)) {
  ccmmf_dir <- config[["ccmmf_dir"]]
}
output_dir <- Sys.getenv("CCMMF_NCC_OUT")
if (!nzchar(output_dir)) {
  output_dir <- file.path(ccmmf_dir, config[["output_dir"]])
}
output_dir <- path.expand(output_dir)

options(arrow.unsafe_metadata = TRUE)

out_path <- output_dir
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

# org_c/org_n must reproduce the C:N of the material on that row, which is what
# drives decomposition. checking against the pooled range across all materials
# would pass a scrambled material to C:N pairing, so bound each row by its own
# material
cn_bounds <- props |>
  dplyr::summarize(cn_lo = min(.data$cn_min), cn_hi = max(.data$cn_max),
                   .by = "material")
cn_check <- dat |>
  dplyr::transmute(.data$material, cn_out = .data$org_c_kg_m2 / .data$org_n_kg_m2) |>
  dplyr::left_join(cn_bounds, by = "material")
bad_cn <- cn_check$cn_out < cn_check$cn_lo - 1e-6 |
  cn_check$cn_out > cn_check$cn_hi + 1e-6
if (any(bad_cn)) {
  worst <- cn_check[which(bad_cn)[1], ]
  fail(sprintf("delivered C:N outside the material's own range on %d rows, e.g. %s at %.1f against %.1f to %.1f",
               sum(bad_cn), worst$material, worst$cn_out, worst$cn_lo, worst$cn_hi))
}

unknown_material <- setdiff(dat$material, props$material)
if (length(unknown_material) > 0) {
  fail("materials not in ca_organic_amendment_properties: ",
       paste(unknown_material, collapse = ", "))
}

# the material draw is uniform over eligible materials, so no material should
# carry a share far from the mean. the threshold has to sit below 2, since the
# bug this guards against (indexing the joined rows, so a two source material is
# drawn twice as often) only reaches about 2x the mean
share <- table(dat$material) / nrow(dat)
if (max(share) > 1.5 * mean(share)) {
  fail(sprintf("material selection is skewed: %s holds %.2f%% against a %.2f%% mean",
               names(share)[which.max(share)], 100 * max(share), 100 * mean(share)))
}

n_ens <- as.integer(config[["n_ensemble"]])
if (dplyr::n_distinct(dat$ens_id) != n_ens) {
  fail(sprintf("found %d ensemble members, expected %d",
               dplyr::n_distinct(dat$ens_id), n_ens))
}

# offsets are signed, so an event can fall outside its anchor's calendar year in
# either direction: row reaches 120 days before a planting date that may already sit
# in the prior year, and hay reaches 14 days after a senescence date that may already
# sit in the following one. one year of slack on each side covers both
expected_years <- as.integer(config[["years"]])
allowed_years <- seq(min(expected_years) - 1L, max(expected_years) + 1L)
extra_years <- setdiff(unique(lubridate::year(dat$date)), allowed_years)
if (length(extra_years) > 0) {
  fail("event dates outside the configured years: ",
       paste(sort(extra_years), collapse = ", "))
}

# the parquet carries the delivered date but not the anchor it was measured from,
# so the per PFT timing rule is asserted against the staged events, where both are
# present. this is the check that a rule was applied as configured rather than
# defaulting to another PFT's window
events_file <- file.path(out_path, "_staging", "_staging_02_events.rds")
if (!file.exists(events_file)) {
  fail("Stage 02 output not found: ", events_file,
       ". Timing rules cannot be verified without it.")
}
staged <- readRDS(events_file)
delta <- as.integer(staged$date - staged$anchor)
out_of_window <- delta < staged$offset_min | delta > staged$offset_max
if (any(out_of_window)) {
  worst <- staged[which(out_of_window)[1], ]
  fail(sprintf("%d events fall outside their PFT's offset window, e.g. %s at %d days against %d to %d",
               sum(out_of_window), worst$pft_group,
               as.integer(worst$date - worst$anchor),
               worst$offset_min, worst$offset_max))
}

realized <- staged |>
  dplyr::summarize(min_off = min(as.integer(.data$date - .data$anchor)),
                   max_off = max(as.integer(.data$date - .data$anchor)),
                   .by = c("pft_group", "anchor_col"))
PEcAn.logger::logger.info("Timing rule per PFT (days from anchor):")
for (i in seq_len(nrow(realized))) {
  PEcAn.logger::logger.info(sprintf("  %s anchored on %s: %d to %d",
                                    realized$pft_group[i], realized$anchor_col[i],
                                    realized$min_off[i], realized$max_off[i]))
}

PEcAn.logger::logger.info("All checks passed")
