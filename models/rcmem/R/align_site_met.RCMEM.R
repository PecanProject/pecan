#' Align site-level meteorological inputs with an RCMEM scenario
#'
#' @param site_met List containing a year vector and optional
#'   suspendedSediment and species_active inputs.
#' @param scenario_years Calendar years used by the RCMEM scenario.
#' @param species_codes Ordered vegetation PFT names.
#'
#' @return A list of site-level inputs aligned to scenario years.
#'
#' @keywords internal
#' @export
align_site_met.RCMEM <- function(
    site_met,
    scenario_years,
    species_codes
) {
  
  if (is.null(site_met$year)) {
    stop("Site met must contain a year vector.")
  }
  
  if (anyNA(site_met$year) ||
      anyDuplicated(site_met$year)) {
    stop("Site met years must be unique and nonmissing.")
  }
  
  if (anyNA(scenario_years) ||
      anyDuplicated(scenario_years)) {
    stop("Scenario years must be unique and nonmissing.")
  }
  
  # Match scenario years to site met years
  idx <- match(scenario_years, site_met$year)
  
  if (anyNA(idx)) {
    stop(
      "Site met is missing scenario years: ",
      paste(scenario_years[is.na(idx)], collapse = ", ")
    )
  }
  
  result <- list()
  
  # ------------------------------------------------------------
  # Suspended sediment concentration
  # ------------------------------------------------------------
  
  if (!is.null(site_met$suspendedSediment)) {
    
    ssc <- site_met$suspendedSediment
    
    if (!is.numeric(ssc) ||
        length(ssc) != length(site_met$year) ||
        any(!is.finite(ssc))) {
      stop(
        "suspendedSediment must be a finite numeric vector ",
        "with one value per site met year."
      )
    }
    
    result$suspendedSediment <- unname(ssc[idx])
  }
  
  # ------------------------------------------------------------
  # Species activity
  # ------------------------------------------------------------
  
  if (!is.null(site_met$species_active)) {
    
    x <- site_met$species_active
    
    if (!is.matrix(x) ||
        !is.numeric(x) ||
        nrow(x) != length(site_met$year) ||
        anyNA(x) ||
        !all(x %in% c(0, 1))) {
      stop(
        "species_active must be a binary numeric matrix ",
        "with one row per site met year."
      )
    }
    
    if (is.null(colnames(x)) ||
        anyDuplicated(colnames(x))) {
      stop(
        "species_active must have unique PFT names as columns."
      )
    }
    
    if (!setequal(colnames(x), species_codes)) {
      stop(
        "species_active columns must match species_codes."
      )
    }
    
    # Check that optional row names agree with the year vector
    if (!is.null(rownames(x)) &&
        !identical(rownames(x), as.character(site_met$year))) {
      stop(
        "species_active row names must match site_met$year."
      )
    }
    
    # Match years and reorder species columns
    result$species_active <- x[
      idx,
      species_codes,
      drop = FALSE
    ]
    
    rownames(result$species_active) <- as.character(scenario_years)
  }
  
  result
}
