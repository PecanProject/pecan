# Leaf area index (LAI) from MSLSP EVImax for CCMMF planting pool initialization.
#
# Mourad et al. (2020) LAI: (max(0, a*sqrt(EVI) - b))^2 with a=2.92, b=0.43.
# The paper uses only the positive part of (a*sqrt(EVI) - b) before squaring;
# EVI is floored at 0.
#
# Planting is not peak canopy. This pipeline scales mslsp_EVImax by k = 0.15
# (OGI-like, ~15% of peak) and passes that into Mourad. k is the planting EVI
# scale; it is not a Mourad coefficient. Same k and EVImax for every PFT.
# PFT only gates whether LAI is computed (row, rice, hay, woody). CLASS is
# not used.
#
# Main entry: compute_lai_from_mslsp(mslsp_EVImax, pft, diagnostics=FALSE).
# Scalar LAI, or diagnostics=TRUE returns a list (LAI, lai_*).
#
# Downstream: sourced by pool_calculations_from_lookup.R for initialize_planting()
# (MSLSP path). Run alone: source this file from R; no env vars for this module.
# Style: docs/SCRIPT_REFACTOR_PROMPT.md in ccmmf-phenology (ASCII, simple sections).

lai_mourad_a <- 2.92
lai_mourad_b <- 0.43
lai_planting_evi_k <- 0.15
lai_default_min <- 0
lai_default_max <- Inf
lai_pfts <- c("row", "rice", "hay", "woody")

compute_lai_from_mslsp <- function(mslsp_EVImax, pft, diagnostics = FALSE) {
  if (!diagnostics) {
    na_out <- NA_real_
  } else {
    na_out <- list(
      LAI = NA_real_,
      lai_evi_value_used = NA_real_,
      lai_k = NA_real_,
      lai_a = NA_real_,
      lai_b = NA_real_,
      lai_min = NA_real_,
      lai_max = NA_real_
    )
  }

  if (length(pft) != 1L || is.na(pft)) {
    return(na_out)
  }
  pft_lc <- tolower(trimws(as.character(pft)))
  if (!nzchar(pft_lc) || !pft_lc %in% lai_pfts) {
    return(na_out)
  }

  evi_num <- suppressWarnings(as.numeric(mslsp_EVImax)[1])
  k <- lai_planting_evi_k
  a <- lai_mourad_a
  b <- lai_mourad_b
  lai <- NA_real_
  if (is.finite(evi_num) && !is.na(k)) {
    evi_num <- pmax(0, evi_num)
    evi_planting <- k * evi_num
    term <- a * sqrt(evi_planting) - b
    lai <- pmax(0, term)^2
    lai <- pmax(lai_default_min, pmin(lai_default_max, lai))
  }

  if (!diagnostics) {
    return(lai)
  }
  list(
    LAI = lai,
    lai_evi_value_used = suppressWarnings(as.numeric(mslsp_EVImax)[1]),
    lai_k = k,
    lai_a = a,
    lai_b = b,
    lai_min = lai_default_min,
    lai_max = lai_default_max
  )
}
