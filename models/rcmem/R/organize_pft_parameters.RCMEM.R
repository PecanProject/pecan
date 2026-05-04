#' Organize sampled PFT parameters for RCMEM
#'
#' Converts PEcAn's PFT-organized parameter draws into the parameter
#' vectors and scalars expected by RCMEM.
#'
#' @param trait.values Named list of sampled parameter vectors, one per PFT.
#' @param settings Data frame containing pft_name, and pft_type for the current site.
#' @param defaults Named list of default RCMEM parameter values.
#'
#' @return Named list of RCMEM model parameters.
#'
#'
#' @keywords internal
#' 
#' @export
organize_pft_parameters.RCMEM <- function(
    trait.values,
    settings,
    defaults = list()
) {
  
  veg_params <- c(
    "bMax",
    "zVegMin",
    "zVegMax",
    "zVegPeak",
    "rootToShoot",
    "rootTurnover",
    "abovegroundTurnover",
    "rootDepthMax",
    "rootPackingDensity"
    # "rootShape"
  )
  
  soil_params <- c(
    "meanOmPackingDensity",
    "meanMineralPackingDensity",
    "omDecayRateFast",
    "omDecayRateSlow",
    "recalcitrantFrac",
    "captureRate",
    # "suspendedSediment",
    "initElv"
  )
  
  # ------------------------------------------------------------
  # Identify vegetation and soil PFTs
  # ------------------------------------------------------------
  
  site_pfts <- settings$run$site$site.pft
  
  veg_names <- unlist(
    site_pfts[grep("^vegetation[0-9]+$", names(site_pfts))],
    use.names = FALSE
  )

  soil_name <- unlist(
    site_pfts[names(site_pfts) == "soil"],
    use.names = FALSE
  )
  
  # veg <- site_pfts |>
  #   dplyr::filter(.data$pft_type == "vegetation")
  # 
  # soil <- site_pfts |>
  #   dplyr::filter(.data$pft_type == "soil")
  
  if (length(veg_names) < 1L) {
    stop("At least one vegetation PFT is required.")
  }
  
  if (length(soil_name) != 1L) {
    stop("Exactly one soil PFT is required.")
  }

  # ------------------------------------------------------------
  # Assemble vegetation parameter vectors
  # ------------------------------------------------------------
  
  result <- list()
  
  for (param in veg_params) {
    
    values <- vapply(
      veg_names,
      function(pft) {
        
        x <- trait.values[[pft]]
        
        if (param %in% names(x)) {
          return(as.numeric(x[[param]]))
        }
        
        # Use defaults for missing sampled parameters
        default <- defaults[[param]]

        if (!is.null(default) && length(default) == 1L) {
          return(as.numeric(default))
        }

        stop(
          "Missing parameter '", param,
          "' for vegetation PFT '", pft, "'."
        )
      },
      numeric(1)
    )
    
    result[[param]] <- unname(values)
  }
  
  # ------------------------------------------------------------
  # Assemble soil parameter scalars
  # ------------------------------------------------------------
  
  soil_values <- trait.values[[soil_name]]
  
  for (param in soil_params) {
  
    if (param %in% names(soil_values)) {
      result[[param]] <- as.numeric(soil_values[[param]])
      
    } else if (!is.null(defaults[[param]])) {
      result[[param]] <- defaults[[param]]
      
    } else {
      stop("Missing soil parameter: ", param)
    }
    
    
    
  }
  
  # ------------------------------------------------------------
  # Organic matter to organic carbon conversion parameters
  # ------------------------------------------------------------
  
  carbon_params <- c("B0", "B1", "B2")
  
  omToOcParams <- list()
  
  for (param in carbon_params) {
    
    trait_name <- paste0("omToOc_", param)
    
    # First priority: sampled soil PFT
    if (trait_name %in% names(soil_values)) {
      
      omToOcParams[[param]] <- as.numeric(
        soil_values[[trait_name]]
      )
      
      # Second priority: study/site defaults
    } else if (!is.null(defaults$omToOcParams[[param]])) {
      
      omToOcParams[[param]] <- defaults$omToOcParams[[param]]
      
    }
  }
  
  # Built-in defaults for required coefficients
  if (is.null(omToOcParams$B0)) {
    omToOcParams$B0 <- 0
  }
  
  if (is.null(omToOcParams$B1)) {
    omToOcParams$B1 <- 0.48
  }
  
  # B2 is intentionally not assigned a default.
  # Its presence determines whether the conversion is quadratic.
  
  # Validate coefficients
  if (any(!vapply(
    omToOcParams,
    function(x) is.numeric(x) &&
    length(x) == 1L &&
    is.finite(x),
    logical(1)
  ))) {
    stop("OM-to-OC coefficients must be finite numeric scalars.")
  }
  
  # Ensure consistent coefficient order
  omToOcParams <- omToOcParams[
    c("B0", "B1", "B2")[
      c("B0", "B1", "B2") %in% names(omToOcParams)
    ]
  ]
  
  # SSC is optional at this stage.
  # It may subsequently be replaced by site-specific met.
  if ("suspendedSediment" %in% names(soil_values)) {
    result$suspendedSediment <- as.numeric(
      soil_values[["suspendedSediment"]]
    )
  } else if (!is.null(defaults[["suspendedSediment"]])) {
    result$suspendedSediment <- defaults[["suspendedSediment"]]
  }
  
  # ------------------------------------------------------------
  # Species identity
  # ------------------------------------------------------------
  
  # Preserve PFT order
  result$species_codes <- veg_names
  
  list(
    model_parameters = result,
    omToOcParams = omToOcParams
  )
}
