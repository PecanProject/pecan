#!/usr/bin/env Rscript

config <- config::get(file = "workflows/ncc-statewide/config.yml",
                      config = Sys.getenv("NCC_PROJECT", "default"))

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

n_ensemble <- as.integer(config[["n_ensemble"]])
p_apply    <- as.numeric(config[["p_apply_default"]])

PEcAn.logger::logger.info(sprintf(
  "Sampling %d ensemble members per cycle with p_apply=%.2f", n_ensemble, p_apply))

# three nested conditional draws: probability of amendment, then material
# / rate / date / %C / C:N given amendment. only rows where the bernoulli
# fires get emitted. all per family lookups live in PEcAn.data.land bundled
# tables so the science ships with package
events <- design |>
  tidyr::crossing(ensemble_member = seq_len(n_ensemble)) |>
  dplyr::mutate(
    applied = stats::rbinom(dplyr::n(), 1, p_apply) == 1L
  ) |>
  dplyr::filter(.data$applied)

n_total <- nrow(design) * n_ensemble
PEcAn.logger::logger.info(sprintf(
  "Probability draw fired %d of %d possible events (%.1f%%)",
  nrow(events), n_total, 100 * nrow(events) / n_total))

if (nrow(events) == 0) {
  PEcAn.logger::logger.severe("No events fired. Check p_apply_default.")
}

events <- events |>
  dplyr::mutate(
    date_offset_days = PEcAn.data.land::sample_ca_compost_date_offset(.data$pft_family),
    date             = .data$anchor - .data$date_offset_days,
    material         = PEcAn.data.land::sample_ca_compost_material(.data$pft_family),
    app_rate_t_ac    = PEcAn.data.land::sample_ca_compost_app_rate(.data$pft_family),
    pct_c            = PEcAn.data.land::sample_ca_compost_pct_c(dplyr::n()),
    cn_ratio         = PEcAn.data.land::sample_ca_compost_cn(dplyr::n()),
    ens_id           = sprintf("ncc_ens_%03d", .data$ensemble_member)
  )

PEcAn.logger::logger.info(sprintf(
  "Sampled %d NCC events. app_rate %.2f-%.2f t/ac, pct_c %.1f-%.1f, cn %.1f-%.1f",
  nrow(events),
  min(events[["app_rate_t_ac"]]), max(events[["app_rate_t_ac"]]),
  min(events[["pct_c"]]),         max(events[["pct_c"]]),
  min(events[["cn_ratio"]]),      max(events[["cn_ratio"]])))

staging_file <- file.path(staging_dir, "_staging_02_events.rds")
saveRDS(events, staging_file)
PEcAn.logger::logger.info("Wrote ", staging_file)
