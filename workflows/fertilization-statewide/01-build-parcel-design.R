#!/usr/bin/env Rscript

config <- config::get(file = "workflows/fertilization-statewide/config.yml",
                      config = Sys.getenv("FERT_PROJECT", "default"))

set.seed(config[["seed"]])

staging_dir <- file.path(config[["output_dir"]], config[["output_subdir"]], "_staging")
dir.create(staging_dir, showWarnings = FALSE, recursive = TRUE)

options(arrow.unsafe_metadata = TRUE)

# strip parenthetical annotations, conjunctions, and punctuation so the
# LandIQ, FREP, and UC ANR strings can be matched on a common key.
normalize_name <- function(s) {
  s |> tolower() |>
    stringr::str_replace_all("\\(.*?\\)", "") |>
    stringr::str_replace_all("grouped for remote sensing only", "") |>
    stringr::str_replace_all("\\bor\\b", "") |>
    stringr::str_replace_all("\\band\\b", "") |>
    stringr::str_replace_all("&", "") |>
    stringr::str_replace_all("[[:punct:]]", " ") |>
    stringr::str_squish()
}

# split slash separated UC ANR or FREP candidates and reattach the shared
# prefix when a part is missing it. e.g. "Plums, dried / fresh" expands to
# c("Plums, dried", "Plums, fresh"). only candidates that match the known
# crop list are kept.
parse_candidates <- function(s, known) {
  if (is.na(s) || nchar(s) == 0) return(character(0))
  parts <- stringr::str_split(s, " ?/ ?")[[1]] |> stringr::str_squish()
  prefix <- if (stringr::str_detect(parts[1], ",")) {
    stringr::str_extract(parts[1], "^[^,]+, ")
  } else ""
  result <- character()
  for (p in parts) {
    if (tolower(p) %in% tolower(known)) {
      result <- c(result, p)
    } else if (nchar(prefix) > 0) {
      combined <- paste0(prefix, p)
      if (tolower(combined) %in% tolower(known)) {
        result <- c(result, combined)
      }
    }
  }
  unique(result)
}

## load lookups
PEcAn.logger::logger.info("Loading bundled lookups and crosswalk")
crosswalk <- readr::read_tsv(config[["crosswalk_path"]], show_col_types = FALSE) |>
  dplyr::select(landiq = "LandIQ Name", frep = "FREP Name", ucanr = "UC ANR")
ca_rates <- PEcAn.data.land::ca_n_application_rate
code_map <- PEcAn.data.land::landiq_crop_mapping_codes

## crosswalk lookup
# resolve each LandIQ CLASS+SUBCLASS code to an N rate envelope by walking
# the crosswalk to UC ANR or FREP names and matching against the bundled
# rate table.
known_crops <- ca_rates$crop
xw_norm <- crosswalk |>
  dplyr::mutate(
    key = normalize_name(.data$landiq),
    candidates = dplyr::coalesce(.data$ucanr, .data$frep)
  )

code_lookup <- code_map |>
  dplyr::mutate(
    code = paste0(.data$CLASS, .data$SUBCLASS),
    key  = normalize_name(.data$subclass_name)
  ) |>
  dplyr::left_join(xw_norm |> dplyr::select("key", "candidates"), by = "key") |>
  dplyr::rowwise() |>
  dplyr::mutate(matched_crops = list(parse_candidates(.data$candidates, known_crops))) |>
  dplyr::ungroup() |>
  dplyr::filter(lengths(.data$matched_crops) > 0) |>
  dplyr::mutate(rates = lapply(.data$matched_crops, function(cc) {
    ca_rates |>
      dplyr::filter(.data$crop %in% cc) |>
      dplyr::summarize(
        min_n_lbs_acre = min(.data$min_n_lbs_acre, na.rm = TRUE),
        max_n_lbs_acre = max(.data$max_n_lbs_acre, na.rm = TRUE)
      )
  })) |>
  tidyr::unnest("rates") |>
  dplyr::select("code", "min_n_lbs_acre", "max_n_lbs_acre")

PEcAn.logger::logger.info(sprintf("Resolved %d LandIQ codes via crosswalk", nrow(code_lookup)))

## load matched product
# parcel_id is stored as a string in the source so cast to int on the way
# in. filter keeps matched cycles with non NA crop class plus a valid EVI
# signal.
read_matched_year <- function(year) {
  fn <- file.path(config[["matched_dir"]],
                  sprintf("assigned_year=%d.parquet", year))
  if (!file.exists(fn)) {
    PEcAn.logger::logger.warn("Missing matched file for year ", year, ": ", fn)
    return(NULL)
  }
  arrow::read_parquet(fn) |>
    dplyr::filter(.data$assigned_by == "matched",
                  !is.na(.data$landiq_CLASS),
                  !is.na(.data$landiq_SUBCLASS),
                  !is.na(.data$landiq_PFT),
                  !is.na(.data$mslsp_EVImax),
                  !is.na(.data$mslsp_EVIamp)) |>
    dplyr::transmute(
      parcel_id = as.integer(.data$parcel_id),
      year      = as.integer(.data$year),
      season    = as.integer(.data$season),
      date      = as.Date(.data$mslsp_OGI),
      code      = paste0(.data$landiq_CLASS, .data$landiq_SUBCLASS),
      PFT       = as.character(.data$landiq_PFT)
    )
}

PEcAn.logger::logger.info("Reading matched LandIQ MSLSP for years: ",
                          paste(config[["years"]], collapse = ", "))
plant <- purrr::map_dfr(config[["years"]], read_matched_year)
PEcAn.logger::logger.info(sprintf("Loaded %d cycles across %d parcels",
                                  nrow(plant), dplyr::n_distinct(plant$parcel_id)))

## subsample
# parcel set is sampled once and applied to all years so the same parcels
# appear in every year.
n_parcels <- config[["n_parcels"]]
if (!is.null(n_parcels) && n_parcels < dplyr::n_distinct(plant$parcel_id)) {
  picked <- plant |>
    dplyr::distinct(.data$parcel_id) |>
    dplyr::slice_sample(n = n_parcels) |>
    dplyr::pull(.data$parcel_id)
  plant <- plant |> dplyr::filter(.data$parcel_id %in% picked)
  PEcAn.logger::logger.info(sprintf("Sampled %d parcels (n_parcels=%d)",
                                    length(picked), n_parcels))
}

## attach rates
# cycles with no resolvable rate are dropped with an audit log. no PFT
# median fallback.
design <- plant |>
  dplyr::left_join(code_lookup, by = "code") |>
  dplyr::mutate(
    rate_source = ifelse(is.na(.data$min_n_lbs_acre), "skip", "crosswalk")
  )

skipped <- design |> dplyr::filter(.data$rate_source == "skip")
if (nrow(skipped) > 0) {
  by_code <- skipped |>
    dplyr::count(.data$code, name = "n_events", sort = TRUE) |>
    head(15)
  PEcAn.logger::logger.warn(sprintf(
    "Dropping %d cycles across %d codes with no resolvable N rate. Top offenders:",
    nrow(skipped), dplyr::n_distinct(skipped$code)
  ))
  for (i in seq_len(nrow(by_code))) {
    PEcAn.logger::logger.warn(sprintf("  %s: %d cycles",
                                      by_code$code[i], by_code$n_events[i]))
  }
}

design <- design |>
  dplyr::filter(.data$rate_source != "skip") |>
  dplyr::select("parcel_id", "year", "season", "date", "code", "PFT",
                "min_n_lbs_acre", "max_n_lbs_acre")

PEcAn.logger::logger.info(sprintf("Design table: %d events, %d parcels, %d years",
                                  nrow(design),
                                  dplyr::n_distinct(design$parcel_id),
                                  dplyr::n_distinct(design$year)))

staging_file <- file.path(staging_dir, "_staging_01_design.rds")
saveRDS(design, staging_file)
PEcAn.logger::logger.info("Wrote ", staging_file)
