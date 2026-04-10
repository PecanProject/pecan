#' Convert fractional NDTI drop to SIPNET tillage effectiveness
#'
#' Maps the fractional NDTI drop produced by the CCMMF NDTI pipeline
#' to \code{tillage_eff_0to1} for use in the PEcAn events JSON schema
#' and written to SIPNET \code{events.in} by
#' \code{\link[PEcAn.SIPNET]{write.events.SIPNET}}.
#'
#' @param delta_ndti numeric vector. Fractional NDTI drop in \[0, 1\],
#'   computed as \code{(max_before_min - min_val) / max_before_min}
#'   from a smoothed NDTI time series over the fallow season.
#' @param no_till_threshold numeric scalar. \code{delta_ndti} values at
#'   or below this threshold are mapped to zero effectiveness (no-till).
#'   Default 0.30, based on Dietze & Kanee (pers. comm.).
#' @param slope numeric scalar. Slope of the linear response above
#'   \code{no_till_threshold}. Default 2.5, which produces a ramp from
#'   0 at \code{delta_ndti = 0.30} to 1 at \code{delta_ndti = 0.70}
#'   (i.e. \code{1 / (0.70 - 0.30)}). Output is clamped to \[0, 1\].
#'
#' @return numeric vector of \code{tillage_eff_0to1} values, same
#'   length as \code{delta_ndti}, clamped to \[0, 1\].
#'   \code{NA} inputs propagate to \code{NA} outputs.
#'
#' @details
#' NDTI drops after tillage as residue is incorporated into soil.
#' The fractional drop over the fallow season is a proxy for tillage
#' intensity. Default thresholds are from Dietze & Kanee (pers. comm.).
#' The slope parameter can be calibrated once field data (SOC or soil
#' respiration) are available.
#'
#' @references
#' Daughtry, C.S.T., Hunt, E.R., Doraiswamy, P.C., McMurtrey, J.E.
#' (2005). Remote sensing the spatial distribution of crop residues.
#' \emph{Agronomy Journal}, 97(3), 864--871.
#' \doi{10.2134/agronj2004.0291}
#'
#' @examples
#' ndti_to_sipnet_tillage(c(0.25, 0.50, 0.80))
#'
#' # Custom slope for sensitivity analysis
#' ndti_to_sipnet_tillage(c(0.25, 0.50, 0.80), slope = 2.0)
#'
#' @export
ndti_to_sipnet_tillage <- function(
    delta_ndti,
    no_till_threshold = 0.30,
    slope             = 2.5
) {
  ## --- input checks ---
  if (!is.numeric(no_till_threshold) || length(no_till_threshold) != 1) {
    PEcAn.logger::logger.severe(
      "no_till_threshold must be a single numeric value."
    )
  }

  if (!is.numeric(slope) || length(slope) != 1 || slope < 0) {
    PEcAn.logger::logger.severe(
      "slope must be a single non-negative numeric value; got ", slope, "."
    )
  }

  delta_ndti <- as.numeric(delta_ndti)

  if (any(delta_ndti < 0, na.rm = TRUE)) {
    PEcAn.logger::logger.warn(
      "Negative delta_ndti values detected; these will be treated as no-till."
    )
  }

  ## --- mapping ---
  tillage_eff <- pmin(pmax((delta_ndti - no_till_threshold) * slope, 0), 1)

  # preserve input NAs
  tillage_eff[is.na(delta_ndti)] <- NA_real_

  tillage_eff
}