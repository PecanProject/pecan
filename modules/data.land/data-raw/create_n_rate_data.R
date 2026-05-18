#!/usr/bin/env Rscript
#
# Build the ca_n_application_rate packaged dataset from the raw
# fertilization spreadsheet. Reads the raw TSV directly, classifies each
# row by stage and unit, converts oz N per tree rows to lb N per acre via
# DEMETER orchard density, sums within year stage rows for crops that
# lack a total season row, and writes both a cached CSV in data-raw/ and
# the packaged .rda in data/ via usethis::use_data

# pull in build time helpers we expose in R/.
source(file.path("R", "tpa_lookup.R"))
source(file.path("R", "oz_per_tree_to_lb_per_acre.R"))

raw_path <- file.path(
  "/projectnb/dietzelab/ccmmf/usr/akash/management/fertilization",
  "CCMMF Fertilization - N_Fertilization.tsv"
)
out_csv <- file.path("data-raw", "n_application_rates.csv")

# 1 lb/acre = 0.112085 g/m^2.
LBS_ACRE_TO_G_M2 <- 0.112085

# stages whose rows sum to an annual total for the same crop. other stage
# tags (e.g. "first season", "first-leaf trees") describe year conditional
# rates and get treated as separate years.
WITHIN_YEAR_STAGES <- c(
  "preplant", "starter", "starter ", "sidedress",
  "topdress", "foliar", "in-season"
)

PEcAn.logger::logger.info("Reading raw fertilization TSV: ", raw_path)
raw <- readr::read_tsv(raw_path, show_col_types = FALSE) |>
  dplyr::select(
    pft_group = "PFT Group",
    crop      = "Crop",
    stage     = "PlantStage",
    min_n     = "MINN",
    max_n     = "MAXN",
    unit      = "Unit",
    source    = "Source"
  )

# drop rows where both min and max are NA. onions and potato in the
# current raw fall into this case and end up flagged below.
usable <- raw |>
  dplyr::filter(!is.na(.data$min_n) | !is.na(.data$max_n)) |>
  dplyr::mutate(
    min_n = dplyr::coalesce(.data$min_n, 0),
    max_n = dplyr::coalesce(.data$max_n, .data$min_n)
  )

# convert any oz N per tree rows to lb N per acre using DEMETER orchard
# density. currently only almond young tree rows hit this branch.
oz_rows <- usable |>
  dplyr::filter(.data$unit == "oz N/tree") |>
  dplyr::mutate(
    tpa = vapply(.data$crop, tpa_lookup, integer(1)),
    min_n = oz_per_tree_to_lb_per_acre(.data$min_n, .data$tpa),
    max_n = oz_per_tree_to_lb_per_acre(.data$max_n, .data$tpa),
    unit = "lbs N/acre"
  ) |>
  dplyr::select(-"tpa")

all_lb <- dplyr::bind_rows(
  usable |> dplyr::filter(.data$unit == "lbs N/acre"),
  oz_rows
) |>
  dplyr::mutate(
    row_kind = dplyr::case_when(
      is.na(.data$stage) | .data$stage == "" ~ "total",
      tolower(.data$stage) %in% .env$WITHIN_YEAR_STAGES ~ "within_year",
      TRUE ~ "year_conditional"
    )
  )

## pick an aggregation strategy per crop. if a total season row exists,
## use it (envelope across total rows). otherwise sum within year stages
## to get an annual total, or envelope across year conditional rows. this
## is the fix that recovers crops the old script silently dropped.
strategy <- all_lb |>
  dplyr::summarize(
    strategy = dplyr::case_when(
      any(.data$row_kind == "total") ~ "envelope_total",
      any(.data$row_kind == "within_year") ~ "sum_stages",
      any(.data$row_kind == "year_conditional") ~ "envelope_year",
      TRUE ~ "drop"
    ),
    .by = c(pft_group, crop)
  )

envelope_total <- all_lb |>
  dplyr::semi_join(
    strategy |> dplyr::filter(.data$strategy == "envelope_total"),
    by = c("pft_group", "crop")
  ) |>
  dplyr::filter(.data$row_kind == "total") |>
  dplyr::summarize(
    min_n_lbs_acre = min(.data$min_n),
    max_n_lbs_acre = max(.data$max_n),
    source = paste(unique(.data$source), collapse = "; "),
    .by = c(pft_group, crop)
  )

sum_stages <- all_lb |>
  dplyr::semi_join(
    strategy |> dplyr::filter(.data$strategy == "sum_stages"),
    by = c("pft_group", "crop")
  ) |>
  dplyr::filter(.data$row_kind == "within_year") |>
  dplyr::summarize(
    min_n_lbs_acre = sum(.data$min_n),
    max_n_lbs_acre = sum(.data$max_n),
    source = paste(unique(.data$source), collapse = "; "),
    .by = c(pft_group, crop)
  )

envelope_year <- all_lb |>
  dplyr::semi_join(
    strategy |> dplyr::filter(.data$strategy == "envelope_year"),
    by = c("pft_group", "crop")
  ) |>
  dplyr::filter(.data$row_kind == "year_conditional") |>
  dplyr::summarize(
    min_n_lbs_acre = min(.data$min_n),
    max_n_lbs_acre = max(.data$max_n),
    source = paste(unique(.data$source), collapse = "; "),
    .by = c(pft_group, crop)
  )

ca_n_application_rate <- dplyr::bind_rows(envelope_total, sum_stages, envelope_year) |>
  dplyr::mutate(
    min_n_g_m2 = round(.data$min_n_lbs_acre * .env$LBS_ACRE_TO_G_M2, 3),
    max_n_g_m2 = round(.data$max_n_lbs_acre * .env$LBS_ACRE_TO_G_M2, 3)
  ) |>
  dplyr::arrange(.data$pft_group, .data$crop)

PEcAn.logger::logger.info(sprintf(
  "Harmonized %d crops into n_application_rates.csv",
  nrow(ca_n_application_rate)
))

# flag crops whose raw rows had NA for both MINN and MAXN.
raw_crops <- unique(raw$crop)
out_crops <- unique(ca_n_application_rate$crop)
dropped <- setdiff(raw_crops, out_crops)
if (length(dropped) > 0) {
  PEcAn.logger::logger.warn(
    "Crops dropped (no usable rate data in raw spreadsheet): ",
    paste(dropped, collapse = ", "),
    ". Their raw rows have NA for both MINN and MAXN."
  )
}

readr::write_csv(ca_n_application_rate, out_csv)
PEcAn.logger::logger.info("Wrote ", out_csv)

usethis::use_data(ca_n_application_rate, overwrite = TRUE)
