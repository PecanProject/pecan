#!/usr/bin/env Rscript

config <- config::get(file = "workflows/ncc-statewide/config.yml",
                      config = Sys.getenv("NCC_PROJECT", "default"))

set.seed(config[["seed"]])

staging_dir <- file.path(config[["output_dir"]], "_staging")
dir.create(staging_dir, showWarnings = FALSE, recursive = TRUE)

options(arrow.unsafe_metadata = TRUE)

# the event date is anchored to green-up (leafonday) from the gap-filled
# phenology product. it carries a green-up for every ag parcel, observed
# where the satellite retrieval succeeded and crop-calendar filled
# otherwise, so this covers the full ~600k ag universe instead of the ~377k
# strict-matched subset. crop class per season comes from the CADWR Land Use
# crops product; pft is derived from class/subclass via cadwr_pfts. the crops
# product's own emergence date is empty statewide, so the gap-filled green-up
# is the only populated anchor available. phenology_source is carried through
# so filled vs observed anchors stay auditable downstream.

# pft_group by (class, subclass); a class-level fallback covers rows whose
# subclass is not specified, since cadwr resolves those by class alone
cadwr <- readr::read_csv(config[["cadwr_pfts_path"]], show_col_types = FALSE) |>
  dplyr::filter(!is.na(.data$pft_group))
pft_by_code <- cadwr |>
  dplyr::transmute(CLASS = .data$class,
                   SUBCLASS = as.integer(.data$subclass),
                   pft_group = .data$pft_group)
pft_by_class <- cadwr |>
  dplyr::count(.data$class, .data$pft_group) |>
  dplyr::slice_max(.data$n, n = 1, by = "class", with_ties = FALSE) |>
  dplyr::transmute(CLASS = .data$class, pft_group_class = .data$pft_group)

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
          CAST(season AS INTEGER) AS season, CLASS, CAST(SUBCLASS AS INTEGER) AS SUBCLASS
   FROM read_parquet('%s') WHERE \"year\" IN (%s) AND CLASS IS NOT NULL",
  config[["crops_path"]], yr_list)) |>
  dplyr::rename(year = "yr") |>
  dplyr::mutate(code = paste0(.data$CLASS, .data$SUBCLASS))

# one green-up per parcel-year. the product carries no season key, so a
# parcel-year with more than one crop cycle anchors every cycle to the same
# date; see Known limitations in the README. the row_number filter only
# resolves the few duplicate rows in the product itself, 26 of 529,285
# site-years in 2016. phenology_source is carried through for audit.
phen <- DBI::dbGetQuery(con, sprintf(
  "SELECT parcel_id, yr, anchor, phenology_source FROM (
     SELECT CAST(site_id AS INTEGER) AS parcel_id, CAST(\"year\" AS INTEGER) AS yr,
            CAST(leafonday AS DATE) AS anchor, phenology_source,
            row_number() OVER (PARTITION BY site_id, \"year\" ORDER BY leafonday) AS rn
     FROM read_parquet('%s/phenology_statewide_*.parquet') WHERE \"year\" IN (%s)
   ) WHERE rn = 1",
  config[["phen_dir"]], yr_list)) |>
  dplyr::rename(year = "yr")

plant <- crops |>
  dplyr::inner_join(phen, by = c("parcel_id", "year")) |>
  dplyr::left_join(pft_by_code, by = c("CLASS", "SUBCLASS")) |>
  dplyr::left_join(pft_by_class, by = "CLASS") |>
  dplyr::mutate(pft_group = dplyr::coalesce(.data$pft_group, .data$pft_group_class))

PEcAn.logger::logger.info(sprintf("Loaded %d cycles across %d parcels (phenology anchored)",
                                  nrow(plant), dplyr::n_distinct(plant$parcel_id)))

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

pft_family <- function(pft) {
  dplyr::case_when(
    pft %in% c("row", "hay", "rice") ~ "annual",
    pft == "woody" ~ "perennial",
    TRUE ~ NA_character_
  )
}

design <- plant |>
  dplyr::mutate(pft_family = pft_family(.data$pft_group))

# non-crop classes (idle, urban, water) resolve to no pft and drop out here
unknown <- design |> dplyr::filter(is.na(.data$pft_family))
if (nrow(unknown) > 0) {
  by_class <- unknown |> dplyr::count(.data$CLASS, sort = TRUE)
  PEcAn.logger::logger.info(sprintf(
    "Dropping %d cycles with no crop pft (non-crop classes):", nrow(unknown)))
  for (i in seq_len(nrow(by_class))) {
    PEcAn.logger::logger.info(sprintf("  CLASS=%s: %d cycles",
                                      by_class$CLASS[i], by_class$n[i]))
  }
}

design <- design |>
  dplyr::filter(!is.na(.data$pft_family)) |>
  dplyr::select("parcel_id", "year", "season", "anchor", "code",
                "pft_family", "phenology_source") |>
  # fixed row order so the per row draws in 02 are reproducible under the seed
  dplyr::arrange(.data$parcel_id, .data$year, .data$season)

PEcAn.logger::logger.info(sprintf("Design table: %d cycles, %d parcels, %d years",
                                  nrow(design),
                                  dplyr::n_distinct(design$parcel_id),
                                  dplyr::n_distinct(design$year)))
PEcAn.logger::logger.info(sprintf("PFT family split: annual=%d, perennial=%d",
                                  sum(design$pft_family == "annual"),
                                  sum(design$pft_family == "perennial")))
src <- design |> dplyr::count(.data$phenology_source, sort = TRUE)
PEcAn.logger::logger.info("Anchor provenance (phenology_source):")
for (i in seq_len(nrow(src))) {
  PEcAn.logger::logger.info(sprintf("  %s: %d cycles (%.1f%%)",
                                    src$phenology_source[i], src$n[i],
                                    100 * src$n[i] / nrow(design)))
}

staging_file <- file.path(staging_dir, "_staging_01_design.rds")
saveRDS(design, staging_file)
PEcAn.logger::logger.info("Wrote ", staging_file)
