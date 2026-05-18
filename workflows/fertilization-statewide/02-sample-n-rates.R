#!/usr/bin/env Rscript

config <- config::get(file = "workflows/fertilization-statewide/config.yml",
                      config = Sys.getenv("FERT_PROJECT", "default"))

set.seed(config[["seed"]])

staging_dir <- file.path(config[["output_dir"]], config[["output_subdir"]], "_staging")
design_file <- file.path(staging_dir, "_staging_01_design.rds")
if (!file.exists(design_file)) {
  PEcAn.logger::logger.severe(
    "Stage 01 output not found: ", design_file,
    ". Run 01-build-parcel-design.R first."
  )
}

PEcAn.logger::logger.info("Reading design from ", design_file)
design <- readRDS(design_file)

n_ensemble <- config[["n_ensemble"]]
PEcAn.logger::logger.info(sprintf(
  "Sampling %d ensemble members across %d design rows",
  n_ensemble, nrow(design)
))

# expand each design row to one row per ensemble member and draw the annual
# N rate uniformly from the resolved min/max envelope.
events <- design |>
  tidyr::crossing(ensemble_member = seq_len(n_ensemble)) |>
  dplyr::mutate(
    annual_n_lb_acre = stats::runif(
      dplyr::n(),
      min = .data$min_n_lbs_acre,
      max = .data$max_n_lbs_acre
    ),
    ens_id = sprintf("fert_ens_%03d", .data$ensemble_member)
  )

PEcAn.logger::logger.info(sprintf(
  "Sampled %d events. annual N range: %.2f to %.2f lb/acre",
  nrow(events), min(events$annual_n_lb_acre), max(events$annual_n_lb_acre)
))

staging_file <- file.path(staging_dir, "_staging_02_events.rds")
saveRDS(events, staging_file)
PEcAn.logger::logger.info("Wrote ", staging_file)
