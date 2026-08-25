#!/usr/bin/env Rscript

config <- config::get(file = "workflows/fertilization-statewide/config.yml",
                      config = Sys.getenv("FERT_PROJECT", "default"))

# paths in config.yml are relative to ccmmf_dir so the yaml stays plain data any
# parser can read. a value from the environment wins and is used as given, so a
# run can point anywhere without editing the file
ccmmf_dir <- Sys.getenv("CCMMF_DIR")
if (!nzchar(ccmmf_dir)) {
  ccmmf_dir <- config[["ccmmf_dir"]]
}
# an override set to an empty string is treated as unset, otherwise file.path
# would build a path rooted at /
resolve_path <- function(key, env_var) {
  p <- Sys.getenv(env_var)
  if (!nzchar(p)) {
    p <- file.path(ccmmf_dir, config[[key]])
  }
  path.expand(p)
}
crops_path      <- resolve_path("crops_path", "CCMMF_CROPS_PATH")
phen_dir        <- resolve_path("phen_dir", "CCMMF_PHEN_DIR")
pft_lookup_path <- resolve_path("pft_lookup_path", "CCMMF_PFT_LOOKUP")
output_dir      <- resolve_path("output_dir", "CCMMF_FERT_OUT")
phen_glob <- Sys.getenv("CCMMF_PHEN_GLOB")
if (!nzchar(phen_glob)) {
  phen_glob <- config[["phen_glob"]]
}

# inputs are required, not optional. away from SCC the defaults will not
# resolve, so name the variable to set rather than failing deeper in a read
inputs <- c(CCMMF_CROPS_PATH = crops_path, CCMMF_PHEN_DIR = phen_dir,
            CCMMF_PFT_LOOKUP = pft_lookup_path)
if (!all(file.exists(inputs))) {
  absent <- inputs[!file.exists(inputs)]
  PEcAn.logger::logger.severe(
    "input not found: ", paste(absent, collapse = ", "),
    ". Set ", paste(names(absent), collapse = ", "), " or CCMMF_DIR.")
}

# log what resolved so a run can be reconstructed from its output alone
PEcAn.logger::logger.info(paste0(
  "\nResolved paths\n",
  "  crops_path      : ", crops_path, "\n",
  "  phen_dir        : ", phen_dir, "\n",
  "  phen_glob       : ", phen_glob, "\n",
  "  pft_lookup_path : ", pft_lookup_path, "\n",
  "  output_dir      : ", output_dir, "\n"), wrap = FALSE)

set.seed(config[["seed"]])

staging_dir <- file.path(output_dir, "_staging")
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

# the event date is the anchor itself; this workflow applies no offset. the
# anchor transition is chosen per PFT so annuals are timed to planting and
# perennials to leaf-on, matching the split the monitoring event products use.
# both come from the gap-filled LandIQ to MSLSP match, which keys every
# transition by (parcel_id, year, season), so cycles join on a real season key.

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
  crops_path, yr_list)) |>
  dplyr::rename(year = "yr") |>
  dplyr::mutate(code = paste0(.data$CLASS, .data$SUBCLASS))

pft_anchor <- unlist(config[["pft_anchor"]])
anchor_cols <- sort(unique(pft_anchor))

# "**" is the LandIQ sentinel for subclass not specified. it becomes NA on both
# sides of the join, so those rows land on the class-level fallback
pft_lookup <- readr::read_csv(pft_lookup_path,
                              show_col_types = FALSE) |>
  dplyr::filter(!is.na(.data$PFT))
pft_by_code <- pft_lookup |>
  dplyr::transmute(CLASS = .data$CLASS,
                   SUBCLASS = as.integer(dplyr::na_if(as.character(.data$SUBCLASS), "**")),
                   pft_group = .data$PFT) |>
  dplyr::distinct()
pft_by_class <- pft_lookup |>
  dplyr::count(.data$CLASS, .data$PFT) |>
  dplyr::slice_max(.data$n, n = 1, by = "CLASS", with_ties = FALSE) |>
  dplyr::transmute(CLASS = .data$CLASS, pft_group_class = .data$PFT)

phen <- DBI::dbGetQuery(con, sprintf(
  "SELECT CAST(parcel_id AS INTEGER) AS parcel_id, CAST(\"year\" AS INTEGER) AS year,
          CAST(season AS INTEGER) AS season, gapfill_date_source, %s
   FROM read_parquet('%s') WHERE \"year\" IN (%s)",
  paste(anchor_cols, collapse = ", "),
  file.path(phen_dir, phen_glob), yr_list))

missing_cols <- setdiff(anchor_cols, names(phen))
if (length(missing_cols) > 0) {
  PEcAn.logger::logger.severe(
    "phenology product has no column(s) named in pft_anchor: ",
    paste(missing_cols, collapse = ", "))
}

# a cycle with no matched phenology row has no anchor, and is dropped rather
# than given a substitute date
plant <- crops |>
  dplyr::inner_join(phen, by = c("parcel_id", "year", "season")) |>
  dplyr::left_join(pft_by_code, by = c("CLASS", "SUBCLASS")) |>
  dplyr::left_join(pft_by_class, by = "CLASS") |>
  dplyr::mutate(pft_group = dplyr::coalesce(.data$pft_group, .data$pft_group_class))
PEcAn.logger::logger.info(sprintf(
  "Anchored %d of %d crop cycles (%.1f%%) across %d parcels",
  nrow(plant), nrow(crops), 100 * nrow(plant) / nrow(crops),
  dplyr::n_distinct(plant$parcel_id)))

# non-crop pfts have no anchor rule. report them so a crop type missing a rule
# is visible rather than silently absent
dropped <- plant |>
  dplyr::filter(!.data$pft_group %in% names(pft_anchor)) |>
  dplyr::count(.data$pft_group, sort = TRUE)
if (nrow(dropped) > 0) {
  PEcAn.logger::logger.info(sprintf(
    "Dropping %d cycles whose pft has no anchor rule:", sum(dropped$n)))
  for (i in seq_len(nrow(dropped))) {
    PEcAn.logger::logger.info(sprintf("  %s: %d cycles",
                                      dropped$pft_group[i], dropped$n[i]))
  }
}
plant <- plant |> dplyr::filter(.data$pft_group %in% names(pft_anchor))

# index a numeric matrix so the anchor stays config driven, not a branch per pft
anchor_idx <- cbind(seq_len(nrow(plant)),
                    match(pft_anchor[plant$pft_group], anchor_cols))
anchor_num <- do.call(cbind, lapply(plant[anchor_cols], as.numeric))
plant$date <- as.Date(anchor_num[anchor_idx], origin = "1970-01-01")

# a matched row is expected to carry every transition, so a NULL anchor means the
# product changed rather than a cycle being legitimately undated
no_anchor <- sum(is.na(plant$date))
if (no_anchor > 0) {
  PEcAn.logger::logger.severe(sprintf(
    "%d cycles have a NULL anchor in the gap-filled product", no_anchor))
}

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
src <- kept |> dplyr::count(.data$gapfill_date_source, sort = TRUE)
PEcAn.logger::logger.info("Anchor provenance (gapfill_date_source):")
for (i in seq_len(nrow(src))) {
  PEcAn.logger::logger.info(sprintf("  %s: %d cycles (%.1f%%)",
                                    src$gapfill_date_source[i], src$n[i],
                                    100 * src$n[i] / nrow(kept)))
}

design <- kept |>
  dplyr::select("parcel_id", "year", "season", "date", "code", "pft_group",
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
