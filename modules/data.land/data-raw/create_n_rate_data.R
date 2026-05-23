#!/usr/bin/env Rscript
#
# Build the ca_n_application_rate packaged dataset from the raw
# fertilization TSV that ships in data-raw/. Classifies each row by
# stage, sums within year stage rows for crops that lack a total season
# row, and writes the packaged .rda via usethis::use_data.

raw_path <- file.path("data-raw", "n_fertilization.tsv")

# stages whose rows sum to an annual total for the same crop. other stage
# tags (e.g. "first season", "first leaf trees") describe year conditional
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

# all rows in the current raw TSV are in lbs N per acre. classify by
# stage so per-stage rows can be aggregated to an annual total when no
# total-season row is available.
all_lb <- usable |>
  dplyr::filter(.data$unit == "lbs N/acre") |>
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
    min_n_g_m2 = round(
      PEcAn.utils::ud_convert(.data$min_n_lbs_acre, "lb/acre", "g/m^2"), 3),
    max_n_g_m2 = round(
      PEcAn.utils::ud_convert(.data$max_n_lbs_acre, "lb/acre", "g/m^2"), 3)
  ) |>
  dplyr::arrange(.data$pft_group, .data$crop)

PEcAn.logger::logger.info(sprintf(
  "Harmonized %d crops", nrow(ca_n_application_rate)))

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

usethis::use_data(ca_n_application_rate, overwrite = TRUE)
