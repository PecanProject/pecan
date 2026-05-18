#!/usr/bin/env Rscript

config <- config::get(file = "workflows/ncc-statewide/config.yml",
                      config = Sys.getenv("NCC_PROJECT", "default"))

set.seed(config[["seed"]])

staging_dir <- file.path(config[["output_dir"]], config[["output_subdir"]], "_staging")
dir.create(staging_dir, showWarnings = FALSE, recursive = TRUE)

options(arrow.unsafe_metadata = TRUE)

# compost application rate is conditioned on PFT family rather than crop
# code so no rate crosswalk is needed at this stage.
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
      anchor    = as.Date(.data$mslsp_OGI),
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

# classify each PFT into annual vs perennial. anything not in this set
# gets dropped with a warning since the rate envelope is per family.
pft_family <- function(pft) {
  dplyr::case_when(
    pft %in% c("row", "vegetable") ~ "annual",
    pft %in% c("woody", "vine")    ~ "perennial",
    TRUE                            ~ NA_character_
  )
}

design <- plant |>
  dplyr::mutate(pft_family = pft_family(.data$PFT))

unknown <- design |> dplyr::filter(is.na(.data$pft_family))
if (nrow(unknown) > 0) {
  by_pft <- unknown |> dplyr::count(.data$PFT, sort = TRUE)
  PEcAn.logger::logger.warn(sprintf(
    "Dropping %d cycles with unknown PFT family. Breakdown:", nrow(unknown)))
  for (i in seq_len(nrow(by_pft))) {
    PEcAn.logger::logger.warn(sprintf("  PFT=%s: %d cycles",
                                      by_pft$PFT[i], by_pft$n[i]))
  }
}

design <- design |>
  dplyr::filter(!is.na(.data$pft_family)) |>
  dplyr::select("parcel_id", "year", "season", "anchor",
                "code", "PFT", "pft_family")

PEcAn.logger::logger.info(sprintf("Design table: %d cycles, %d parcels, %d years",
                                  nrow(design),
                                  dplyr::n_distinct(design$parcel_id),
                                  dplyr::n_distinct(design$year)))
PEcAn.logger::logger.info(sprintf("PFT family split: annual=%d, perennial=%d",
                                  sum(design$pft_family == "annual"),
                                  sum(design$pft_family == "perennial")))

staging_file <- file.path(staging_dir, "_staging_01_design.rds")
saveRDS(design, staging_file)
PEcAn.logger::logger.info("Wrote ", staging_file)
