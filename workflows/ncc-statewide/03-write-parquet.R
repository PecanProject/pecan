#!/usr/bin/env Rscript

config <- config::get(file = "workflows/ncc-statewide/config.yml",
                      config = Sys.getenv("NCC_PROJECT", "default"))

# 1 t/acre = 224.1702 g/m^2 = 0.2241702 kg/m^2.
T_ACRE_TO_KG_M2 <- 0.2241702

staging_dir <- file.path(config[["output_dir"]], config[["output_subdir"]], "_staging")
events_file <- file.path(staging_dir, "_staging_02_events.rds")
if (!file.exists(events_file)) {
  PEcAn.logger::logger.severe(
    "Stage 02 output not found: ", events_file,
    ". Run 02-sample-ncc-events.R first."
  )
}

PEcAn.logger::logger.info("Reading events from ", events_file)
events <- readRDS(events_file)

# org_c = app_rate * (pct_c / 100); org_n = org_c / cn. nh4 and no3 stay
# zero here because compost releases organic N that mineralizes through
# the soil pool in SIPNET rather than as direct mineral N.
# TODO: in v2, sample %C and C:N jointly since they covary through %N.
out <- events |>
  dplyr::mutate(
    org_c_kg_m2  = .data$app_rate_t_ac * .env$T_ACRE_TO_KG_M2 * (.data$pct_c / 100),
    org_n_kg_m2  = .data$org_c_kg_m2 / .data$cn_ratio,
    nh4_n_kg_m2  = 0,
    no3_n_kg_m2  = 0,
    ncc_subtype  = "compost"
  ) |>
  dplyr::transmute(
    parcel_id    = as.integer(.data$parcel_id),
    ens_id       = .data$ens_id,
    date         = as.Date(.data$date),
    .data$material,
    .data$org_c_kg_m2,
    .data$org_n_kg_m2,
    .data$nh4_n_kg_m2,
    .data$no3_n_kg_m2,
    .data$ncc_subtype,
    crop_code    = .data$code,
    PFT          = .data$PFT
  )

out_path <- file.path(config[["output_dir"]], config[["output_subdir"]])
dir.create(out_path, showWarnings = FALSE, recursive = TRUE)

## clean prior shards
existing <- list.files(out_path, pattern = "\\.parquet$", full.names = TRUE)
if (length(existing) > 0) {
  PEcAn.logger::logger.info(sprintf("Removing %d existing parquet shards", length(existing)))
  unlink(existing)
}

# partition by parcel_id range and write one parquet per batch named
# <pid_min>_<pid_max>.parquet.
all_parcels <- sort(unique(out[["parcel_id"]]))
batch_size  <- as.integer(config[["batch_size"]])
n_batches   <- ceiling(length(all_parcels) / batch_size)
batches     <- split(all_parcels, ceiling(seq_along(all_parcels) / batch_size))

PEcAn.logger::logger.info(sprintf(
  "Writing %d rows across %d parcel batches (batch_size=%d) to %s",
  nrow(out), n_batches, batch_size, out_path))

# prefer ZSTD; fall back to snappy when zstd is not in the local arrow
# build.
parquet_codec <- if (arrow::codec_is_available("zstd")) "ZSTD" else "SNAPPY"
PEcAn.logger::logger.info("Parquet compression codec: ", parquet_codec)

write_batch <- function(pids) {
  shard <- out |> dplyr::filter(.data$parcel_id %in% pids)
  pid_min <- min(shard[["parcel_id"]])
  pid_max <- max(shard[["parcel_id"]])
  fn <- file.path(out_path, sprintf("%d_%d.parquet", pid_min, pid_max))
  arrow::write_parquet(shard, fn, compression = parquet_codec)
  fn
}

workers <- as.integer(config[["workers"]])
if (workers > 1) {
  PEcAn.logger::logger.info(sprintf("Using mclapply with %d workers", workers))
  written <- parallel::mclapply(batches, write_batch, mc.cores = workers)
} else {
  written <- lapply(batches, write_batch)
}

PEcAn.logger::logger.info(sprintf(
  "Done. wrote %d shards, %d total rows, parcels=%d, materials=%d, ensemble=%d",
  length(written), nrow(out),
  dplyr::n_distinct(out[["parcel_id"]]),
  dplyr::n_distinct(out[["material"]]),
  dplyr::n_distinct(out[["ens_id"]])))
