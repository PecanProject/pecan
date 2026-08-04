#!/usr/bin/env Rscript

config <- config::get(file = "workflows/ncc-statewide/config.yml",
                      config = Sys.getenv("NCC_PROJECT", "default"))

set.seed(config[["seed"]])

staging_dir <- file.path(config[["output_dir"]], "_staging")
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
# so material alone would cross pair one source's C:N with another's rate.
# wood is excluded from annuals: high C:N immobilizes more N than a row crop
# rotation absorbs in one season
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

allowed_classes <- list(
  annual = c("food", "yard", "ag"),
  perennial = c("food", "yard", "ag", "wood")
)
family_structure <- c(annual = "rows", perennial = "trees")

# a class outside this set would join cleanly but be excluded from every pool
# without trace, so check it rather than let materials disappear
unknown_class <- setdiff(amendments$material_class, unlist(allowed_classes))
if (length(unknown_class) > 0L) {
  PEcAn.logger::logger.severe(
    "material_class values not covered by allowed_classes: ",
    paste(sort(unknown_class), collapse = ", "))
}

# group each family's eligible rows by material. drawing the joined rows
# directly would weight a material by how many sources report it, so the three
# two-source materials would come up twice as often as any other
pool_by_material <- function(family) {
  rows <- which(amendments$crop_structure == family_structure[[family]] &
                amendments$material_class %in% allowed_classes[[family]])
  split(rows, amendments$material[rows])
}
families <- stats::setNames(names(allowed_classes), names(allowed_classes))
pools <- lapply(families, pool_by_material)

# uniform over materials, then uniform over that material's sources, so source
# disagreement stays in the ensemble without biasing which material is picked.
# drawn per family in one pass: a closure per event row costs millions of calls
# at statewide scale
draw_material_rows <- function(pool, n) {
  n_source <- lengths(pool)
  flat <- unlist(pool, use.names = FALSE)
  offset <- cumsum(c(0L, n_source[-length(n_source)]))
  material <- sample.int(length(pool), n, replace = TRUE)
  within <- ceiling(stats::runif(n) * n_source[material])
  flat[offset[material] + within]
}

events$mat_idx <- NA_integer_
for (fam in names(pools)) {
  sel <- which(events$pft_family == fam)
  events$mat_idx[sel] <- draw_material_rows(pools[[fam]], length(sel))
}

mat_cols <- amendments[events$mat_idx,
                       c("material", "material_class",
                         "app_rate_min", "app_rate_max",
                         "n_pct", "pan_pct",
                         "cn_min", "cn_max")]
events <- dplyr::bind_cols(events, mat_cols)

# date offset windows are working assumptions: perennials get fall/winter
# application (Niederholzer 2019, UCCE), annuals get a wider pre planting
# window (Fulford et al 2023, CA processing tomatoes)
ANNUAL_OFFSET_MIN <- 14L
ANNUAL_OFFSET_MAX <- 180L
PERENNIAL_OFFSET_MIN <- 30L
PERENNIAL_OFFSET_MAX <- 210L

events <- events |>
  dplyr::mutate(
    u_rate = stats::runif(dplyr::n()),
    u_cn = stats::runif(dplyr::n()),
    app_rate_lb_acre = .data$app_rate_min + .data$u_rate * (.data$app_rate_max - .data$app_rate_min),
    cn_ratio = .data$cn_min + .data$u_cn * (.data$cn_max - .data$cn_min),
    date_offset_days = ifelse(
      .data$pft_family == "annual",
      sample(ANNUAL_OFFSET_MIN:ANNUAL_OFFSET_MAX, dplyr::n(), replace = TRUE),
      sample(PERENNIAL_OFFSET_MIN:PERENNIAL_OFFSET_MAX, dplyr::n(), replace = TRUE)
    ),
    date = .data$anchor - .data$date_offset_days,
    ens_id = sprintf("ens_%03d", .data$ensemble_member)
  )

PEcAn.logger::logger.info(sprintf(
  "Sampled %d compost events. app_rate %.0f to %.0f lb/acre, n_pct %.2f to %.2f, cn %.1f to %.1f",
  nrow(events),
  min(events[["app_rate_lb_acre"]]), max(events[["app_rate_lb_acre"]]),
  min(events[["n_pct"]]), max(events[["n_pct"]]),
  min(events[["cn_ratio"]]), max(events[["cn_ratio"]])))

staging_file <- file.path(staging_dir, "_staging_02_events.rds")
saveRDS(events, staging_file)
PEcAn.logger::logger.info("Wrote ", staging_file)
