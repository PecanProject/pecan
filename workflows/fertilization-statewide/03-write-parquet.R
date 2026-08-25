#!/usr/bin/env Rscript

config <- config::get(file = "workflows/fertilization-statewide/config.yml",
                      config = Sys.getenv("FERT_PROJECT", "default"))

# override set to an empty string is treated as unset
ccmmf_dir <- Sys.getenv("CCMMF_DIR")
if (!nzchar(ccmmf_dir)) {
  ccmmf_dir <- config[["ccmmf_dir"]]
}
output_dir <- Sys.getenv("CCMMF_FERT_OUT")
if (!nzchar(output_dir)) {
  output_dir <- file.path(ccmmf_dir, config[["output_dir"]])
}
output_dir <- path.expand(output_dir)

staging_dir <- file.path(output_dir, "_staging")
events_file <- file.path(staging_dir, "_staging_02_events.rds")
if (!file.exists(events_file)) {
  PEcAn.logger::logger.severe(
    "Stage 02 output not found: ", events_file,
    ". Run 02-sample-n-rates.R first."
  )
}

PEcAn.logger::logger.info("Reading events from ", events_file)
events <- readRDS(events_file)

# synthetic N is split between nh4 and no3 using nh4_fraction (share going
# to nh4, rest goes to no3). org_c and org_n stay zero here; compost rows
# come in via ncc workflow and end up unioned into same fertilization
# parquet by cleaner
nh4_frac <- config[["nh4_fraction"]]

out <- events |>
  dplyr::mutate(
    total_n_kg_m2 = PEcAn.utils::ud_convert(.data$annual_n_lb_acre, "lb/acre", "kg/m^2"),
    nh4_n_kg_m2 = .data$total_n_kg_m2 * .env$nh4_frac,
    no3_n_kg_m2 = .data$total_n_kg_m2 * (1 - .env$nh4_frac),
    org_c_kg_m2 = 0,
    org_n_kg_m2 = 0
  ) |>
  dplyr::transmute(
    parcel_id = as.integer(.data$parcel_id),
    ens_id = .data$ens_id,
    date = as.Date(.data$date),
    .data$nh4_n_kg_m2,
    .data$no3_n_kg_m2,
    .data$org_c_kg_m2,
    .data$org_n_kg_m2,
    crop_code = .data$code
  )

out_path <- output_dir
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
batch_size  <- config[["batch_size"]]
n_batches   <- ceiling(length(all_parcels) / batch_size)
batches     <- split(all_parcels, ceiling(seq_along(all_parcels) / batch_size))

PEcAn.logger::logger.info(sprintf(
  "Writing %d rows across %d parcel batches (batch_size=%d) to %s",
  nrow(out), n_batches, batch_size, out_path
))

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

# mclapply does not raise when a worker fails: it returns a try-error for an R
# level error and NULL for a killed worker, which is the realistic out of memory
# case. write_batch returns the shard path, so anything that is not a path failed.
# a try-error is itself a character vector, so is.character alone lets it through
failed <- vapply(
  written,
  function(x) {
    is.null(x) ||
      inherits(x, "try-error") ||
      !is.character(x) ||
      length(x) != 1L ||
      !file.exists(x)
  },
  logical(1)
)
if (any(failed)) {
  PEcAn.logger::logger.severe(sprintf(
    "%d of %d shard writes failed", sum(failed), length(written)))
}

PEcAn.logger::logger.info(sprintf(
  "Done. wrote %d shards, %d total rows, parcels=%d, years=%d, ensemble=%d",
  length(written), nrow(out),
  dplyr::n_distinct(out[["parcel_id"]]),
  dplyr::n_distinct(format(out[["date"]], "%Y")),
  dplyr::n_distinct(out[["ens_id"]])
))
