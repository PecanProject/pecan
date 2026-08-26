#!/usr/bin/env Rscript

config <- config::get(file = "workflows/ncc-statewide/config.yml",
                      config = Sys.getenv("NCC_PROJECT", "default"))

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
output_dir      <- resolve_path("output_dir", "CCMMF_NCC_OUT")
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

# the anchor transition is chosen per PFT: compost is timed against planting for
# row and rice, leaf-on for woody, harvest for hay. the gap-filled match carries
# every transition keyed (parcel_id, year, season), so cycles join their anchor
# on a real season key rather than by rank. pft comes from the same crop code
# table the monitoring products use, so a rule keyed on PFT means the same thing
# on both sides.

# one table drives both timing and rate, so adding a PFT is a config change
timing <- purrr::map_dfr(config[["pft_timing"]], tibble::as_tibble,
                         .id = "pft_group")
required_timing <- c("anchor_col", "offset_min", "offset_max", "crop_structure")
if (!all(required_timing %in% names(timing))) {
  PEcAn.logger::logger.severe(
    "pft_timing entries must define: ", paste(required_timing, collapse = ", "))
}
if (any(timing$offset_min > timing$offset_max)) {
  PEcAn.logger::logger.severe("pft_timing has an offset window with min > max")
}

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

anchor_cols <- sort(unique(timing$anchor_col))
phen_files <- file.path(phen_dir, phen_glob)
phen <- DBI::dbGetQuery(con, sprintf(
  "SELECT CAST(parcel_id AS INTEGER) AS parcel_id, CAST(\"year\" AS INTEGER) AS year,
          CAST(season AS INTEGER) AS season, gapfill_date_source, %s
   FROM read_parquet('%s') WHERE \"year\" IN (%s)",
  paste(anchor_cols, collapse = ", "), phen_files, yr_list))

missing_cols <- setdiff(anchor_cols, names(phen))
if (length(missing_cols) > 0) {
  PEcAn.logger::logger.severe(
    "phenology product has no column(s) named in pft_timing$anchor_col: ",
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
  "Anchored %d of %d crop cycles (%.1f%%); %d had no matched phenology row",
  nrow(plant), nrow(crops), 100 * nrow(plant) / nrow(crops),
  nrow(crops) - nrow(plant)))

# non-crop pfts have no timing rule. report them so a crop type missing a rule
# is visible rather than silently absent
dropped <- plant |>
  dplyr::filter(!.data$pft_group %in% timing$pft_group) |>
  dplyr::count(.data$pft_group, sort = TRUE)
if (nrow(dropped) > 0) {
  PEcAn.logger::logger.info(sprintf(
    "Dropping %d cycles whose pft has no timing rule:", sum(dropped$n)))
  for (i in seq_len(nrow(dropped))) {
    PEcAn.logger::logger.info(sprintf("  %s: %d cycles",
                                      dropped$pft_group[i], dropped$n[i]))
  }
}
plant <- plant |> dplyr::inner_join(timing, by = "pft_group")

# index a numeric matrix so the anchor stays config driven, not a branch per pft
anchor_idx <- cbind(seq_len(nrow(plant)), match(plant$anchor_col, anchor_cols))
anchor_num <- do.call(cbind, lapply(plant[anchor_cols], as.numeric))
plant$anchor <- as.Date(anchor_num[anchor_idx], origin = "1970-01-01")

# a matched row is expected to carry every transition, so a NULL anchor means the
# product changed rather than a cycle being legitimately undated
no_anchor <- sum(is.na(plant$anchor))
if (no_anchor > 0) {
  PEcAn.logger::logger.severe(sprintf(
    "%d cycles have a NULL anchor in the gap-filled product", no_anchor))
}

## subsample
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

design <- plant |>
  dplyr::select("parcel_id", "year", "season", "anchor", "code", "pft_group",
                "anchor_col", "crop_structure", "offset_min", "offset_max",
                "gapfill_date_source") |>
  # fixed row order so the per row draws in 02 are reproducible under the seed
  dplyr::arrange(.data$parcel_id, .data$year, .data$season)

PEcAn.logger::logger.info(sprintf("Design table: %d cycles, %d parcels, %d years",
                                  nrow(design),
                                  dplyr::n_distinct(design$parcel_id),
                                  dplyr::n_distinct(design$year)))

pft_n <- design |> dplyr::count(.data$pft_group, sort = TRUE)
PEcAn.logger::logger.info("Cycles per PFT:")
for (i in seq_len(nrow(pft_n))) {
  PEcAn.logger::logger.info(sprintf("  %s: %d cycles (%.1f%%)",
                                    pft_n$pft_group[i], pft_n$n[i],
                                    100 * pft_n$n[i] / nrow(design)))
}

src <- design |> dplyr::count(.data$gapfill_date_source, sort = TRUE)
PEcAn.logger::logger.info("Anchor provenance (gapfill_date_source):")
for (i in seq_len(nrow(src))) {
  PEcAn.logger::logger.info(sprintf("  %s: %d cycles (%.1f%%)",
                                    src$gapfill_date_source[i], src$n[i],
                                    100 * src$n[i] / nrow(design)))
}

staging_file <- file.path(staging_dir, "_staging_01_design.rds")
saveRDS(design, staging_file)
PEcAn.logger::logger.info("Wrote ", staging_file)
