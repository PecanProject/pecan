#!/usr/bin/env Rscript

event_dir <- "_events"
dir.create(event_dir, showWarnings = FALSE, recursive = TRUE)
parquet_dir <- "_output"
site_ids <- c(0, 1, 10, 100, 1000, 100005)
# site_ids <- seq(1, 100)
# site_ids <- seq(1, 500)

ens_ids <- PEcAn.data.land::get_event_ensemble_ids(parquet_dir = parquet_dir)

events_ensemble_manifest <- dplyr::as_tibble(ens_ids) |>
  dplyr::mutate(
    ensemble_id = sprintf("ens_%03d", dplyr::row_number()),
    json_path = file.path(
      .env$event_dir,
      sprintf("events_%s.json", .data$ensemble_id)
    )
  ) |>
  dplyr::relocate("ensemble_id", "json_path")

events_files <- PEcAn.data.land::event_parquet_to_json(
  parquet_dir = parquet_dir,
  events_ensemble_manifest = events_ensemble_manifest,
  site_ids = site_ids
)

message("Validating event files")
pb <- utils::txtProgressBar(0, nrow(events_files))
for (i in seq_len(nrow(events_files))) {
  path <- events_files[["json_path"]][[i]]
  PEcAn.data.land::validate_events_json(path, schema_version = "0.1.2", verbose = TRUE)
  utils::setTxtProgressBar(pb, i)
}
close(pb)
