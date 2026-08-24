#!/usr/bin/env Rscript

config <- config::get(file = "workflows/fertilization-statewide/config.yml",
                      config = Sys.getenv("FERT_PROJECT", "default"))

set.seed(config[["seed"]])

staging_dir <- file.path(config[["output_dir"]], "_staging")
dir.create(staging_dir, showWarnings = FALSE, recursive = TRUE)

options(arrow.unsafe_metadata = TRUE)

# strip parenthetical annotations, conjunctions, and punctuation so the crop
# name strings from the three sources can be matched on a common key.
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
# resolve each CADWR CLASS+SUBCLASS code to an N rate envelope by walking
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

PEcAn.logger::logger.info(sprintf("Resolved %d CADWR codes via crosswalk", nrow(code_lookup)))

# the event date is anchored to green-up (leafonday) from the gap-filled
# phenology product, observed where the satellite retrieval succeeded and
# crop-calendar filled otherwise, so this covers the full ~600k ag universe
# instead of the ~377k strict-matched subset. crop class per season comes
# from the CADWR Land Use crops product. the crops product's own emergence
# date is empty statewide, so the gap-filled green-up is the only populated
# anchor available.

years <- config[["years"]]
PEcAn.logger::logger.info("Reading crops and gap-filled phenology for years: ",
                          paste(years, collapse = ", "))

# read via duckdb: it casts the bigint parcel_id cleanly and fast, where an
# arrow collect returns integer64 that stalls the downstream integer coercion
con <- DBI::dbConnect(duckdb::duckdb())
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
yr_list <- paste(years, collapse = ",")

# crop class per real season from the CADWR Land Use crops product; the 4-slot
# season structure is mostly NA-padded, keep rows carrying a crop class
crops <- DBI::dbGetQuery(con, sprintf(
  "SELECT CAST(parcel_id AS INTEGER) AS parcel_id, CAST(\"year\" AS INTEGER) AS yr,
          CAST(season AS INTEGER) AS season, CLASS,
          TRY_CAST(NULLIF(NULLIF(TRIM(CAST(SUBCLASS AS VARCHAR)), '**'), '') AS INTEGER) AS SUBCLASS
   FROM read_parquet('%s') WHERE \"year\" IN (%s) AND CLASS IS NOT NULL",
  config[["crops_path"]], yr_list)) |>
  dplyr::rename(year = "yr") |>
  dplyr::mutate(code = paste0(.data$CLASS, .data$SUBCLASS))

# the phenology product has no season key, but from 2018 on it carries a second
# green-up for most double-crop parcels, so rank green-ups within a parcel-year
# and match the nth crop cycle to the nth green-up rather than collapsing to the
# earliest. phenology_source is carried through for audit.
phen_anchor_col <- config[["phen_anchor_col"]]
phen_raw <- DBI::dbGetQuery(con, sprintf(
  "SELECT * FROM read_parquet('%s') WHERE \"year\" IN (%s)",
  file.path(config[["phen_dir"]], config[["phen_glob"]]), yr_list))
phen_id_col <- if ("parcel_id" %in% names(phen_raw)) "parcel_id" else "site_id"
phen_source_col <- if ("phenology_source" %in% names(phen_raw)) {
  "phenology_source"
} else if ("gapfill_date_source" %in% names(phen_raw)) {
  "gapfill_date_source"
} else {
  NA_character_
}
phen <- phen_raw |>
  dplyr::transmute(
    parcel_id = as.integer(.data[[phen_id_col]]),
    year = as.integer(.data$year),
    date = as.Date(.data[[phen_anchor_col]]),
    phenology_source = if (is.na(phen_source_col)) {
      NA_character_
    } else {
      as.character(.data[[phen_source_col]])
    }
  ) |>
  dplyr::filter(!is.na(.data$date)) |>
  dplyr::arrange(.data$parcel_id, .data$year, .data$date) |>
  dplyr::mutate(phen_rank = dplyr::row_number(), .by = c("parcel_id", "year"))

# where a parcel-year has fewer green-ups than crop cycles, the later cycles
# reuse the last available one
phen_max <- phen |>
  dplyr::summarize(max_rank = max(.data$phen_rank), .by = c("parcel_id", "year"))

plant <- crops |>
  dplyr::mutate(season_rank = dplyr::dense_rank(.data$season),
                .by = c("parcel_id", "year")) |>
  dplyr::inner_join(phen_max, by = c("parcel_id", "year")) |>
  dplyr::mutate(phen_rank = pmin(.data$season_rank, .data$max_rank)) |>
  dplyr::inner_join(phen, by = c("parcel_id", "year", "phen_rank"))
PEcAn.logger::logger.info(sprintf("Loaded %d cycles across %d parcels (phenology anchored)",
                                  nrow(plant), dplyr::n_distinct(plant$parcel_id)))

## subsample
# parcel set is sampled once and applied to all years so the same parcels
# appear in every year.
n_parcels <- config[["n_parcels"]]
if (!is.null(n_parcels) && n_parcels < dplyr::n_distinct(plant$parcel_id)) {
  # sort before sampling: duckdb returns rows in a parallelism dependent order,
  # so an unsorted frame gives a different subsample on every run despite the seed
  picked <- plant |>
    dplyr::distinct(.data$parcel_id) |>
    dplyr::arrange(.data$parcel_id) |>
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
    rate_source = dplyr::case_when(
      is.na(.data$min_n_lbs_acre) ~ "skip_no_rate",
      .data$min_n_lbs_acre == 0 & .data$max_n_lbs_acre == 0 ~ "skip_zero_envelope",
      TRUE ~ "crosswalk"
    )
  )

# log both kinds of drops separately so it's clear which are missing data
# vs which are intentional zero envelopes (e.g. Alfalfa, legumes that fix
# their own N and carry a cited 0 to 0 rate)
unresolved <- design |> dplyr::filter(.data$rate_source == "skip_no_rate")
if (nrow(unresolved) > 0) {
  by_code <- unresolved |>
    dplyr::count(.data$code, name = "n_events", sort = TRUE) |>
    head(15)
  PEcAn.logger::logger.warn(sprintf(
    "Dropping %d cycles across %d codes with no resolvable N rate. Top offenders:",
    nrow(unresolved), dplyr::n_distinct(unresolved$code)
  ))
  for (i in seq_len(nrow(by_code))) {
    PEcAn.logger::logger.warn(sprintf("  %s: %d cycles",
                                      by_code$code[i], by_code$n_events[i]))
  }
}

zero_env <- design |> dplyr::filter(.data$rate_source == "skip_zero_envelope")
if (nrow(zero_env) > 0) {
  by_code <- zero_env |>
    dplyr::count(.data$code, name = "n_events", sort = TRUE) |>
    head(15)
  PEcAn.logger::logger.info(sprintf(
    "Dropping %d cycles across %d codes with a cited 0 to 0 N rate (no synthetic application). Top:",
    nrow(zero_env), dplyr::n_distinct(zero_env$code)
  ))
  for (i in seq_len(nrow(by_code))) {
    PEcAn.logger::logger.info(sprintf("  %s: %d cycles",
                                      by_code$code[i], by_code$n_events[i]))
  }
}

kept <- design |> dplyr::filter(.data$rate_source == "crosswalk")
src <- kept |> dplyr::count(.data$phenology_source, sort = TRUE)
PEcAn.logger::logger.info("Anchor provenance (phenology_source):")
for (i in seq_len(nrow(src))) {
  PEcAn.logger::logger.info(sprintf("  %s: %d cycles (%.1f%%)",
                                    src$phenology_source[i], src$n[i],
                                    100 * src$n[i] / nrow(kept)))
}

design <- kept |>
  dplyr::select("parcel_id", "year", "season", "date", "code",
                "min_n_lbs_acre", "max_n_lbs_acre") |>
  # fixed row order so the per row draws in 02 are reproducible under the seed
  dplyr::arrange(.data$parcel_id, .data$year, .data$season)

PEcAn.logger::logger.info(sprintf("Design table: %d events, %d parcels, %d years",
                                  nrow(design),
                                  dplyr::n_distinct(design$parcel_id),
                                  dplyr::n_distinct(design$year)))

staging_file <- file.path(staging_dir, "_staging_01_design.rds")
saveRDS(design, staging_file)
PEcAn.logger::logger.info("Wrote ", staging_file)
