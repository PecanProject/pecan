#!/usr/bin/env Rscript

events_dir <- "/projectnb/dietzelab/ccmmf/management/event_files"
harvest_files <- list.files(events_dir, "harvest_statewide_.*.parquet", full.names = TRUE)
planting_files <- list.files(events_dir, "planting_statewide_.*.parquet", full.names = TRUE)
phenology_files <- list.files(events_dir, "phenology_statewide_.*.parquet", full.names = TRUE)
tillage_files <- list.files(events_dir, "tillage_statewide_.*.parquet", full.names = TRUE)

outdir <- "_output"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

message("Writing harvest output")
harvest <- arrow::open_dataset(harvest_files, format = "parquet") |>
  dplyr::mutate(
    site_id = as.integer(site_id),
    date = as.Date(date)
  ) |>
  dplyr::arrange(.data$site_id) |>
  arrow::write_parquet(
    file.path(outdir, "harvest.parquet"),
    compression = "ZSTD"
  )

message("Writing planting output")
planting <- arrow::open_dataset(planting_files, format = "parquet") |>
  dplyr::mutate(
    site_id = as.integer(site_id),
    date = as.Date(date)
  ) |>
  dplyr::rename(
    crop_code = "code",
    leaf_c_kg_m2 = "C_LEAF",
    wood_c_kg_m2 = "C_STEM",
    fine_root_c_kg_m2 = "C_FINEROOT",
    coarse_root_c_kg_m2 = "C_COARSEROOT",
    leaf_n_kg_m2 = "N_LEAF",
    wood_n_kg_m2 = "N_STEM",
    fine_root_n_kg_m2 = "N_FINEROOT",
    coarse_root_n_kg_m2 = "N_COARSEROOT"
  ) |>
  arrow::write_parquet(
    file.path(outdir, "planting.parquet"),
    compression = "ZSTD"
  )

pct_to_tillage <- function(x) {
  xfrac <- x / 100
  x1 <- pmax(xfrac - 0.3, 0.0)
  pmin(x1 / 0.7, 1.0)
}

message("Writing tillage output")
tillage <- arrow::open_dataset(tillage_files, format = "parquet") |>
  dplyr::filter(
    is.finite(.data$ndti_pct_change),
    .data$ndti_pct_change >= 0
  ) |>
  dplyr::mutate(
    site_id = as.integer(site_id),
    tillage_eff_0to1 = pct_to_tillage(ndti_pct_change),
    date = as.Date(.data$OGMn_date)
  ) |>
  dplyr::select(
    "site_id",
    "date",
    "tillage_eff_0to1"
  ) |>
  arrow::write_parquet(
    file.path(outdir, "tillage.parquet"),
    compression = "ZSTD"
  )

message("Writing phenology output")
phenology <- arrow::open_dataset(phenology_files, format = "parquet")
leafon <- phenology |>
  dplyr::select("site_id", date = "leafonday") |>
  dplyr::mutate(
    site_id = as.integer(.data$site_id),
    date = as.Date(.data$date)
  ) |>
  arrow::write_parquet(
    file.path(outdir, "leafon.parquet"),
    compression = "ZSTD"
  )
leafoff <- phenology |>
  dplyr::select("site_id", date = "leafoffday") |>
  dplyr::mutate(
    site_id = as.integer(.data$site_id),
    date = as.Date(.data$date)
  ) |>
  arrow::write_parquet(
    file.path(outdir, "leafoff.parquet"),
    compression = "ZSTD"
  )
