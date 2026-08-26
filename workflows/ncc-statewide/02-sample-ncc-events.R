#!/usr/bin/env Rscript

config <- config::get(file = "workflows/ncc-statewide/config.yml",
                      config = Sys.getenv("NCC_PROJECT", "default"))

# an override set to an empty string is treated as unset
ccmmf_dir <- Sys.getenv("CCMMF_DIR")
if (!nzchar(ccmmf_dir)) {
  ccmmf_dir <- config[["ccmmf_dir"]]
}
output_dir <- Sys.getenv("CCMMF_NCC_OUT")
if (!nzchar(output_dir)) {
  output_dir <- file.path(ccmmf_dir, config[["output_dir"]])
}
output_dir <- path.expand(output_dir)

set.seed(config[["seed"]])

staging_dir <- file.path(output_dir, "_staging")
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
p_apply <- as.numeric(config[["p_apply_default"]])

PEcAn.logger::logger.info(sprintf(
  "Sampling %d ensemble members per cycle with p_apply=%.2f", n_ensemble, p_apply))

# bernoulli gate first so most parcel year ensemble combinations drop out
# before heavier material lookup
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

# join on source as well as material: some materials carry two source rows,
# so material alone would cross pair one source's C:N with another's rate
props <- PEcAn.data.land::ca_organic_amendment_properties
rates <- PEcAn.data.land::ca_organic_amendment_app_rate
amendments <- dplyr::inner_join(props, rates, by = c("material", "source"))

# every app_rate row must match a properties row, else the inner_join drops it
unmatched <- dplyr::anti_join(rates, props, by = c("material", "source"))
if (nrow(unmatched) > 0L) {
  PEcAn.logger::logger.severe(
    "app_rate rows with no matching properties row: ",
    paste(unique(unmatched$material), collapse = ", ")
  )
}

# which rate applies is a property of the pft, not of a binary annual/perennial
# split. every material is eligible for every structure: SIPNET already slows
# decomposition of high C:N material through calcCNEffect, so screening materials
# out here would double count what the model does
structures <- sort(unique(design$crop_structure))
unknown_structure <- setdiff(structures, amendments$crop_structure)
if (length(unknown_structure) > 0) {
  PEcAn.logger::logger.severe(
    "crop_structure in pft_timing has no rows in ca_organic_amendment_app_rate: ",
    paste(unknown_structure, collapse = ", "))
}

# group each structure's rows by material. drawing the joined rows directly would
# weight a material by how many sources report it, so the three two-source
# materials would come up twice as often as any other
pool_by_material <- function(structure) {
  rows <- which(amendments$crop_structure == structure)
  split(rows, amendments$material[rows])
}
pools <- lapply(stats::setNames(structures, structures), pool_by_material)

# uniform over materials, then uniform over that material's sources, so source
# disagreement stays in the ensemble without biasing which material is picked.
# drawn per structure in one pass: a closure per event row costs millions of
# calls at statewide scale
draw_material_rows <- function(pool, n) {
  n_source <- lengths(pool)
  flat <- unlist(pool, use.names = FALSE)
  offset <- cumsum(c(0L, n_source[-length(n_source)]))
  material <- sample.int(length(pool), n, replace = TRUE)
  within <- ceiling(stats::runif(n) * n_source[material])
  flat[offset[material] + within]
}

events$mat_idx <- NA_integer_
for (structure in structures) {
  sel <- which(events$crop_structure == structure)
  events$mat_idx[sel] <- draw_material_rows(pools[[structure]], length(sel))
}

mat_cols <- amendments[events$mat_idx,
                       c("material", "material_class",
                         "app_rate_min", "app_rate_max",
                         "n_pct",
                         "cn_min", "cn_max")]
events <- dplyr::bind_cols(events, mat_cols)

# signed offsets let a rule place the event before the anchor or after it.
# discrete uniform, inclusive of both bounds
events <- events |>
  dplyr::mutate(
    u_rate = stats::runif(dplyr::n()),
    u_cn = stats::runif(dplyr::n()),
    app_rate_lb_acre = .data$app_rate_min + .data$u_rate * (.data$app_rate_max - .data$app_rate_min),
    cn_ratio = .data$cn_min + .data$u_cn * (.data$cn_max - .data$cn_min),
    date_offset_days = .data$offset_min +
      floor(stats::runif(dplyr::n()) * (.data$offset_max - .data$offset_min + 1)),
    date = .data$anchor + .data$date_offset_days,
    ens_id = sprintf("ens_%03d", .data$ensemble_member)
  )

PEcAn.logger::logger.info(sprintf(
  "Sampled %d compost events. app_rate %.0f to %.0f lb/acre, n_pct %.2f to %.2f, cn %.1f to %.1f",
  nrow(events),
  min(events[["app_rate_lb_acre"]]), max(events[["app_rate_lb_acre"]]),
  min(events[["n_pct"]]), max(events[["n_pct"]]),
  min(events[["cn_ratio"]]), max(events[["cn_ratio"]])))

# realized windows show each rule was applied as configured
win <- events |>
  dplyr::summarize(n = dplyr::n(),
                   min_off = min(.data$date_offset_days),
                   max_off = max(.data$date_offset_days),
                   .by = "pft_group")
PEcAn.logger::logger.info("Realized offset window per PFT (days from anchor):")
for (i in seq_len(nrow(win))) {
  PEcAn.logger::logger.info(sprintf("  %s: %d to %d over %d events",
                                    win$pft_group[i], win$min_off[i],
                                    win$max_off[i], win$n[i]))
}

staging_file <- file.path(staging_dir, "_staging_02_events.rds")
saveRDS(events, staging_file)
PEcAn.logger::logger.info("Wrote ", staging_file)
