#' Align site-specific met inputs with an RCMEM scenario
#'
#' @param site_met Named list containing `year` and optionally
#'   `suspendedSediment` and `species_presence`.
#' @param scenario_years Numeric vector of scenario calendar years.
#' @param species_codes Character vector of vegetation PFT names
#'   in the order expected by RCMEM.
#'
#' @return Named list of aligned site-specific met inputs.
#' @export
align_site_met.RCMEM <- function(
    site_met,
    scenario_years,
    species_codes
) {
  
  if (!is.list(site_met) || is.data.frame(site_met)) {
    stop("site_met must be a named list.")
  }
  
  allowed <- c(
    "year",
    "suspendedSediment",
    "species_presence"
  )
  
  if (is.null(names(site_met)) ||
      anyNA(names(site_met)) ||
      any(!nzchar(names(site_met))) ||
      anyDuplicated(names(site_met)) ||
      any(!names(site_met) %in% allowed)) {
    stop("site_met contains invalid or duplicate field names.")
  }
  
  site_years <- site_met$year
  
  if (is.null(site_years) ||
      !is.numeric(site_years) ||
      anyNA(site_years) ||
      anyDuplicated(site_years)) {
    stop("site_met$year must contain unique numeric years.")
  }
  
  if (!is.numeric(scenario_years) ||
      anyNA(scenario_years) ||
      anyDuplicated(scenario_years)) {
    stop("scenario_years must contain unique numeric years.")
  }
  
  idx <- match(scenario_years, site_years)
  
  if (anyNA(idx)) {
    stop(
      "Site met is missing scenario years: ",
      paste(scenario_years[is.na(idx)], collapse = ", ")
    )
  }
  
  result <- list()
  
  # ------------------------------------------------------------
  # Suspended sediment
  # ------------------------------------------------------------
  
  if (!is.null(site_met$suspendedSediment)) {
    
    ssc <- site_met$suspendedSediment
    
    if (!is.numeric(ssc) ||
        !is.null(dim(ssc)) ||
        length(ssc) != length(site_years) ||
        any(!is.finite(ssc))) {
      stop(
        "suspendedSediment must be a finite numeric vector ",
        "with one value per site year."
      )
    }
    
    result$suspendedSediment <- unname(ssc[idx])
  }
  
  # ------------------------------------------------------------
  # Species presence
  # ------------------------------------------------------------
  
  if (!is.null(site_met$species_presence)) {
    
    x <- site_met$species_presence
    
    if (!is.matrix(x) ||
        !is.numeric(x) ||
        nrow(x) != length(site_years) ||
        anyNA(x) ||
        !all(x %in% c(0, 1))) {
      stop(
        "species_presence must be a binary numeric matrix ",
        "with years as rows and species as columns."
      )
    }
    
    if (is.null(colnames(x)) ||
        anyNA(colnames(x)) ||
        anyDuplicated(colnames(x)) ||
        !setequal(colnames(x), species_codes)) {
      stop(
        "species_presence columns must match species_codes."
      )
    }
    
    if (!is.null(rownames(x)) &&
        !identical(
          rownames(x),
          as.character(site_years)
        )) {
      stop(
        "species_presence row names must match site_met$year."
      )
    }
    
    result$species_presence <- x[
      idx,
      species_codes,
      drop = FALSE
    ]
    
    rownames(result$species_presence) <-
      as.character(scenario_years)
  }
  
  result
}