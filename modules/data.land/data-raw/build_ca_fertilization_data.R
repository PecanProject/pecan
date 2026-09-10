#!/usr/bin/env Rscript
# build the three ca fertilization datasets from the curated source tsvs. run
# from the package root; create_ca_n_application_rate.R and
# create_ca_organic_amendment.R then write the .rda from the csvs this produces.

n_fert_tsv  <- file.path("data-raw", "n_fertilization.tsv")
compost_tsv <- file.path("data-raw", "organic_amendments.tsv")

# only area rates convert to g/m2; rows in any other unit are dropped and reported
n_rate_unit <- "lbs N/acre"

# within season stages: a crop reported only by stage has these summed into a
# season total. orchard age rows ("year 1", "years 7-15") are enveloped instead.
within_year_stages <- c("preplant", "starter", "sidedress", "topdress",
                        "foliar", "in-season")

# curated source urls shortened to author year citations
source_citation <- c(
  "https://escholarship.org/uc/item/5mk2q1sm" = "Rosenstock et al., 2013",
  "https://extensionpubs.unl.edu/publication/g2222/na/html/view" = "Eghball, UNL Extension",
  "https://content.ces.ncsu.edu/compost-production-and-use-in-sustainable-farming-systems" = "Rynk, NC State Extension"
)
cite_source <- function(x) unname(dplyr::coalesce(source_citation[x], x))

# lbs N/acre to g N/m2, always through ud_convert
to_g_m2 <- function(x) round(PEcAn.utils::ud_convert(x, "lb/acre", "g/m^2"), 3)

# read a tsv and return the required columns, fail if any is missing
read_required <- function(path, cols) {
  dat <- readr::read_tsv(path, show_col_types = FALSE)
  missing <- setdiff(cols, colnames(dat))
  if (length(missing) > 0) {
    PEcAn.logger::logger.severe(
      basename(path), " missing required columns: ", paste(missing, collapse = ", "))
  }
  dat[, cols]
}

n_fert_cols <- c("PFT Group", "Crop", "PlantStage", "Season",
                 "MINN", "MAXN", "Unit", "Source", "Notes")

read_n_fert <- function(path) {
  read_required(path, n_fert_cols) |>
    dplyr::rename(
      pft_group = "PFT Group", crop = "Crop", stage = "PlantStage",
      season = "Season", min_n = "MINN", max_n = "MAXN", unit = "Unit",
      source = "Source", notes = "Notes")
}

# tag each area rate row by how its crop reports N; drop and report other units
classify_rows <- function(dat) {
  dropped <- dat |> dplyr::filter(!is.na(.data$unit), .data$unit != n_rate_unit)
  if (nrow(dropped) > 0) {
    PEcAn.logger::logger.warn(
      nrow(dropped), " N rate rows are not ", n_rate_unit, " and are dropped: ",
      paste(sort(unique(dropped$crop)), collapse = ", "))
  }
  dat |>
    dplyr::filter(.data$unit == n_rate_unit,
                  !is.na(.data$min_n) | !is.na(.data$max_n)) |>
    dplyr::mutate(
      min_n = dplyr::coalesce(.data$min_n, 0),
      max_n = dplyr::coalesce(.data$max_n, .data$min_n),
      row_kind = dplyr::case_when(
        is.na(.data$stage) | .data$stage == "" ~ "total",
        tolower(.data$stage) %in% .env$within_year_stages ~ "within_year",
        TRUE ~ "year_conditional"),
      source = cite_source(.data$source))
}

# one N range per crop: envelope the whole season rows, else sum the within season
# stages, else envelope the orchard age rows
build_n_rates <- function(dat) {
  classified <- classify_rows(dat)

  strategy <- classified |>
    dplyr::summarize(
      strategy = dplyr::case_when(
        any(.data$row_kind == "total") ~ "envelope_total",
        any(.data$row_kind == "within_year") ~ "sum_stages",
        any(.data$row_kind == "year_conditional") ~ "envelope_year",
        TRUE ~ "drop"),
      .by = c("pft_group", "crop"))

  crops_for <- function(strat) dplyr::filter(strategy, .data$strategy == strat)

  envelope_total <- classified |>
    dplyr::semi_join(crops_for("envelope_total"), by = c("pft_group", "crop")) |>
    dplyr::filter(.data$row_kind == "total") |>
    dplyr::summarize(min_n_lbs_acre = min(.data$min_n),
                     max_n_lbs_acre = max(.data$max_n),
                     source = paste(unique(.data$source), collapse = "; "),
                     .by = c("pft_group", "crop"))

  sum_stages <- classified |>
    dplyr::semi_join(crops_for("sum_stages"), by = c("pft_group", "crop")) |>
    dplyr::filter(.data$row_kind == "within_year") |>
    dplyr::summarize(min_n_lbs_acre = sum(.data$min_n),
                     max_n_lbs_acre = sum(.data$max_n),
                     source = paste(unique(.data$source), collapse = "; "),
                     .by = c("pft_group", "crop"))

  envelope_year <- classified |>
    dplyr::semi_join(crops_for("envelope_year"), by = c("pft_group", "crop")) |>
    dplyr::filter(.data$row_kind == "year_conditional") |>
    dplyr::summarize(min_n_lbs_acre = min(.data$min_n),
                     max_n_lbs_acre = max(.data$max_n),
                     source = paste(unique(.data$source), collapse = "; "),
                     .by = c("pft_group", "crop"))

  dplyr::bind_rows(envelope_total, sum_stages, envelope_year) |>
    dplyr::mutate(min_n_g_m2 = to_g_m2(.data$min_n_lbs_acre),
                  max_n_g_m2 = to_g_m2(.data$max_n_lbs_acre)) |>
    dplyr::arrange(.data$pft_group, .data$crop)
}

compost_cols <- c(
  "Material", "material_class", "C_MIN (C:N)", "C_MAX (C:N)", "C_Avg (C:N)",
  "Total N (%)", "4 week PAN (%)", "LowerN/HigherN",
  "RowsMIN_AppRate (lbs/acre)", "RowsMAX_AppRate (lbs/acre)",
  "RowsMIN_Total_N (lbs N/acre)", "RowsMAX_Total_N (lbs N/acre)",
  "TreesMIN_AppRate (lbs/acre)", "TreesMAX_AppRate (lbs/acre)",
  "TreesMIN_Total_N (lbs N/acre)", "TreesMAX_Total_N (lbs N/acre)", "Source")

# intrinsic amendment properties, one row per (material, source)
build_amendment_properties <- function(raw) {
  raw |>
    dplyr::transmute(
      material       = .data$Material,
      material_class = .data$material_class,
      cn_min         = .data$`C_MIN (C:N)`,
      cn_max         = .data$`C_MAX (C:N)`,
      cn_avg         = .data$`C_Avg (C:N)`,
      n_pct          = as.numeric(.data$`Total N (%)`),
      pan_pct        = as.numeric(.data$`4 week PAN (%)`),
      n_class        = .data$`LowerN/HigherN`,
      source         = cite_source(trimws(.data$Source)))
}

# application rates, long by crop structure; the wide Rows/Trees columns unpivot
# into rows/trees records
build_amendment_app_rate <- function(raw) {
  structure_rows <- function(prefix, label) {
    raw |>
      dplyr::transmute(
        material             = .data$Material,
        crop_structure       = label,
        app_rate_min         = .data[[paste0(prefix, "MIN_AppRate (lbs/acre)")]],
        app_rate_max         = .data[[paste0(prefix, "MAX_AppRate (lbs/acre)")]],
        total_n_min_lbs_acre = .data[[paste0(prefix, "MIN_Total_N (lbs N/acre)")]],
        total_n_max_lbs_acre = .data[[paste0(prefix, "MAX_Total_N (lbs N/acre)")]],
        source               = cite_source(trimws(.data$Source)))
  }
  dplyr::bind_rows(structure_rows("Rows", "rows"),
                   structure_rows("Trees", "trees")) |>
    dplyr::mutate(total_n_min_g_m2 = to_g_m2(.data$total_n_min_lbs_acre),
                  total_n_max_g_m2 = to_g_m2(.data$total_n_max_lbs_acre)) |>
    dplyr::arrange(.data$material, .data$crop_structure)
}

PEcAn.logger::logger.info("building ca fertilization datasets")

n_fert  <- read_n_fert(n_fert_tsv)
n_rates <- build_n_rates(n_fert)

# completeness: every crop with a usable area rate must survive to the output
lost <- setdiff(unique(n_fert$crop[n_fert$unit == n_rate_unit]), n_rates$crop)
if (length(lost) > 0) {
  PEcAn.logger::logger.warn("crops with no usable rate: ", paste(lost, collapse = ", "))
}

compost    <- read_required(compost_tsv, compost_cols)
properties <- build_amendment_properties(compost)
app_rate   <- build_amendment_app_rate(compost)

no_class <- properties$material[is.na(properties$material_class)]
if (length(no_class) > 0) {
  PEcAn.logger::logger.warn("materials with no material_class: ", paste(no_class, collapse = ", "))
}

readr::write_csv(n_rates, file.path("data-raw", "ca_n_application_rate.csv"))
readr::write_csv(properties, file.path("data-raw", "ca_organic_amendment_properties.csv"))
readr::write_csv(app_rate, file.path("data-raw", "ca_organic_amendment_app_rate.csv"))

PEcAn.logger::logger.info(
  nrow(n_rates), " crops, ", nrow(properties), " amendment rows, ",
  nrow(app_rate), " app rate rows written")
