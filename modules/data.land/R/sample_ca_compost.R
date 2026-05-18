#' Sample California compost amendment %C
#'
#' Draws n values of compost carbon content (%C, dry weight basis) from
#' the bundled truncated normal distribution at
#' ca_compost_pct_c_distribution.
#'
#' The default range (15 to 30%, mean 20%, sd 5) reflects CA finished
#' compost characterizations: Bernard et al. 2023 (central coast
#' vineyard, 14 to 15% C); Sullivan, Bary, Miller and Brewer 2018 (OSU
#' EM9217 proficiency program, median 15% TOC); CalRecycle generic
#' compost roughly 20 to 23% C.
#'
#' @param n integer. Number of draws.
#' @param params optional list overriding the bundled distribution. Must
#'   contain numeric a, b, mean, sd.
#'
#' @return numeric vector of length n, each value in [a, b].
#'
#' @examples
#' sample_ca_compost_pct_c(5)
#'
#' @export
sample_ca_compost_pct_c <- function(n, params = NULL) {
  p <- params %||% as.list(PEcAn.data.land::ca_compost_pct_c_distribution)
  truncnorm::rtruncnorm(n, a = p$a, b = p$b, mean = p$mean, sd = p$sd)
}

#' Sample California compost C:N ratio
#'
#' Draws n values of compost C:N from the bundled truncated normal
#' distribution at ca_compost_cn_distribution.
#'
#' Default range (8 to 25, mean 12, sd 4) reflects the bimodal CA compost
#' distribution documented in the CDFA Healthy Soils Program white paper
#' (Geisseler et al.): manure dominant compost clusters at C:N at or
#' below 11 while plant dominant compost sits above 11. HSP 2026 Practice
#' Guidelines require applied C:N at or below 25.
#'
#' @param n integer. Number of draws.
#' @param params optional list overriding the bundled distribution.
#'
#' @return numeric vector of length n.
#'
#' @examples
#' sample_ca_compost_cn(5)
#'
#' @export
sample_ca_compost_cn <- function(n, params = NULL) {
  p <- params %||% as.list(PEcAn.data.land::ca_compost_cn_distribution)
  truncnorm::rtruncnorm(n, a = p$a, b = p$b, mean = p$mean, sd = p$sd)
}

#' Sample California compost application rate by PFT family
#'
#' Draws n application rates (dry weight tons per acre) uniformly from
#' the per family envelope at ca_compost_app_rate_envelope.
#'
#' Envelopes follow CDFA Healthy Soils Program white paper Table 2 plus
#' HSP 2026 Practice Guidelines plus USDA NRCS Conservation Practice
#' Standard 336 Soil Carbon Amendment (2022). Defaults: annual crops 3
#' to 8 t/ac dry, perennial crops 2 to 6 t/ac dry.
#'
#' @param pft_family character vector. PFT family for each draw. Must be
#'   one of "annual", "perennial". Length 1 broadcasts to length n.
#' @param n integer. Number of draws. If pft_family has length > 1 then
#'   n must equal length(pft_family).
#'
#' @return numeric vector of length n in tons per acre dry weight.
#'
#' @examples
#' sample_ca_compost_app_rate("annual", 5)
#' sample_ca_compost_app_rate(c("annual", "perennial", "annual"), 3)
#'
#' @export
sample_ca_compost_app_rate <- function(pft_family, n = length(pft_family)) {
  if (length(pft_family) == 1 && n > 1) {
    pft_family <- rep(pft_family, n)
  }
  if (length(pft_family) != n) {
    PEcAn.logger::logger.severe(
      "length(pft_family) must equal n; got ", length(pft_family), " vs ", n)
  }
  env <- PEcAn.data.land::ca_compost_app_rate_envelope
  out <- numeric(n)
  for (fam in unique(pft_family)) {
    if (!fam %in% env$pft_family) {
      PEcAn.logger::logger.severe(
        "Unknown pft_family '", fam, "'. Supported: ",
        paste(env$pft_family, collapse = ", "))
    }
    row <- env[env$pft_family == fam, , drop = FALSE]
    idx <- which(pft_family == fam)
    out[idx] <- stats::runif(length(idx), min = row$min_t_ac, max = row$max_t_ac)
  }
  out
}

#' Sample California compost application date offset by PFT family
#'
#' Returns n integer day offsets (subtract from the cycle anchor,
#' typically MSLSP mslsp_OGI) drawn uniformly from the per family
#' calendar window at ca_compost_calendar_window.
#'
#' Defaults: annual crops 7 to 28 days before the anchor (CDFA HSP Box 1,
#' applied and incorporated before planting); perennial crops 60 to 180
#' days before the anchor (dormant season application, UC ANR cost
#' studies plus Bernard et al. 2023 vineyard timing of Nov 9 and Jan 10).
#'
#' @param pft_family character vector. PFT family for each draw.
#' @param n integer. Number of draws.
#'
#' @return integer vector of day offsets.
#'
#' @examples
#' sample_ca_compost_date_offset("annual", 5)
#'
#' @export
sample_ca_compost_date_offset <- function(pft_family, n = length(pft_family)) {
  if (length(pft_family) == 1 && n > 1) {
    pft_family <- rep(pft_family, n)
  }
  if (length(pft_family) != n) {
    PEcAn.logger::logger.severe(
      "length(pft_family) must equal n; got ", length(pft_family), " vs ", n)
  }
  win <- PEcAn.data.land::ca_compost_calendar_window
  out <- integer(n)
  for (fam in unique(pft_family)) {
    if (!fam %in% win$pft_family) {
      PEcAn.logger::logger.severe(
        "Unknown pft_family '", fam, "'. Supported: ",
        paste(win$pft_family, collapse = ", "))
    }
    row <- win[win$pft_family == fam, , drop = FALSE]
    idx <- which(pft_family == fam)
    out[idx] <- sample(row$offset_days_min:row$offset_days_max,
                       length(idx), replace = TRUE)
  }
  out
}

#' Sample California compost material class by PFT family
#'
#' Returns n material classes drawn from the per family whitelist at
#' ca_compost_material_whitelist. Uniform within the whitelist.
#'
#' Whitelist follows the CalRecycle taxonomy (14 CCR section 17852 plus
#' SB 1383 dominant feedstocks: green, food, wood, yard, ag, biosolids).
#' Defaults: annual crops can receive green / food / ag; perennial crops
#' can receive green / food / yard. Biosolids are excluded from food
#' crop parcels for regulatory compliance; wood waste is deferred since
#' it immobilizes N.
#'
#' @param pft_family character vector. PFT family for each draw.
#' @param n integer. Number of draws.
#'
#' @return character vector of material classes.
#'
#' @examples
#' sample_ca_compost_material("annual", 5)
#'
#' @export
sample_ca_compost_material <- function(pft_family, n = length(pft_family)) {
  if (length(pft_family) == 1 && n > 1) {
    pft_family <- rep(pft_family, n)
  }
  if (length(pft_family) != n) {
    PEcAn.logger::logger.severe(
      "length(pft_family) must equal n; got ", length(pft_family), " vs ", n)
  }
  wl <- PEcAn.data.land::ca_compost_material_whitelist
  out <- character(n)
  for (fam in unique(pft_family)) {
    allowed <- wl$material_class[wl$pft_family == fam]
    if (length(allowed) == 0) {
      PEcAn.logger::logger.severe(
        "No material whitelist for pft_family '", fam, "'. Supported: ",
        paste(unique(wl$pft_family), collapse = ", "))
    }
    idx <- which(pft_family == fam)
    out[idx] <- sample(allowed, length(idx), replace = TRUE)
  }
  out
}

# small null fallback helper.
`%||%` <- function(x, y) if (is.null(x)) y else x
