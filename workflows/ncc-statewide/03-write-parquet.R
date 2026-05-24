#!/usr/bin/env Rscript

config <- config::get(file = "workflows/ncc-statewide/config.yml",
                      config = Sys.getenv("NCC_PROJECT", "default"))

staging_dir <- file.path(config[["output_dir"]], "_staging")
events_file <- file.path(staging_dir, "_staging_02_events.rds")
if (!file.exists(events_file)) {
  PEcAn.logger::logger.severe(
    "Stage 02 output not found: ", events_file,
    ". Run 02-sample-ncc-events.R first."
  )
}

PEcAn.logger::logger.info("Reading events from ", events_file)
events <- readRDS(events_file)

# split total N using PAN (plant available N at 4 weeks). pan_pct can be
# negative for high C:N materials (immobilization); clamp to 0 in that
# case since SIPNET doesn't take negative minN. total C is just bulk
# material carbon, doesn't depend on PAN
out <- events |>
  dplyr::mutate(
    dry_mass_kg_m2 = PEcAn.utils::ud_convert(.data$app_rate_lb_acre, "lb/acre", "kg/m^2"),
    total_n_kg_m2 = .data$dry_mass_kg_m2 * (.data$n_pct / 100),
    pan_frac = pmax(0, .data$pan_pct / 100),
    nh4_n_kg_m2 = .data$total_n_kg_m2 * .data$pan_frac,
    org_n_kg_m2 = .data$total_n_kg_m2 * (1 - .data$pan_frac),
    org_c_kg_m2 = .data$total_n_kg_m2 * .data$cn_ratio,
    no3_n_kg_m2 = 0
  ) |>
  dplyr::transmute(
    parcel_id = as.integer(.data$parcel_id),
    ens_id = .data$ens_id,
    date = as.Date(.data$date),
    .data$nh4_n_kg_m2,
    .data$no3_n_kg_m2,
    .data$org_c_kg_m2,
    .data$org_n_kg_m2,
    crop_code = .data$code,
    .data$material
  )

out_path <- config[["output_dir"]]
dir.create(out_path, showWarnings = FALSE, recursive = TRUE)

## clean prior shards
existing <- list.files(out_path, pattern = "\\.parquet$", full.names = TRUE)
if (length(existing) > 0) {
  PEcAn.logger::logger.info(sprintf("Removing %d existing parquet shards", length(existing)))
  unlink(existing)
}

# partition by parcel_id range and write one parquet per batch named
# <pid_min>_<pid_max>.parquet
all_parcels <- sort(unique(out[["parcel_id"]]))
batch_size  <- as.integer(config[["batch_size"]])
n_batches   <- ceiling(length(all_parcels) / batch_size)
batches     <- split(all_parcels, ceiling(seq_along(all_parcels) / batch_size))

PEcAn.logger::logger.info(sprintf(
  "Writing %d rows across %d parcel batches (batch_size=%d) to %s",
  nrow(out), n_batches, batch_size, out_path))

# prefer ZSTD; fall back to snappy when zstd is not in the local arrow
# build
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
  "Done. wrote %d shards, %d total rows, parcels=%d, ensemble=%d",
  length(written), nrow(out),
  dplyr::n_distinct(out[["parcel_id"]]),
  dplyr::n_distinct(out[["ens_id"]])))
