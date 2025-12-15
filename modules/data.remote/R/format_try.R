#' Convert TRY database data to PEcAn meta-analysis format
#' 
#' @param try_data TRY data frame
#' @param trait_map Named vector: TRY TraitName -> PEcAn vname
#' @param citation_id Default citation ID
#' @export
format_try_for_ma <- function(try_data, trait_map, citation_id = 999) {
  
  # Filter trait data only
  if (!"TraitID" %in% names(try_data)) {
    stop("TRY data must contain 'TraitID' column")
  }
  
  trait_rows <- !is.na(try_data$TraitID)
  if (sum(trait_rows) == 0) {
    warning("No trait data found")
    return(data.frame())
  }
  
  data <- try_data[trait_rows, ]
  
  # Create output
  result <- data.frame(
    id = if ("ObsDataID" %in% names(data)) data$ObsDataID else 1:nrow(data),
    citation_id = citation_id,
    site_id = 1,
    treatment_id = NA,
    name = NA,
    date = NA,
    time = NA,
    cultivar_id = NA,
    specie_id = NA,
    mean = if ("StdValue" %in% names(data)) as.numeric(data$StdValue) else NA,
    statname = if ("ErrorRisk" %in% names(data)) "SE" else NA,
    stat = if ("ErrorRisk" %in% names(data)) as.numeric(data$ErrorRisk) else NA,
    n = if ("Replicates" %in% names(data)) as.numeric(data$Replicates) else NA,
    vname = if ("TraitName" %in% names(data)) {
      sapply(data$TraitName, function(x) {
        if (x %in% names(trait_map)) trait_map[x] else x
      })
    } else NA,
    month = NA,
    greenhouse = FALSE,
    control = FALSE,
    stringsAsFactors = FALSE
  )
  
  message("Converted ", nrow(result), " trait records")
  return(result)
}

#' Example trait mapping
#' @export
try_trait_mapping <- c(
  "Leaf nitrogen (N) content per leaf dry mass" = "leaf_N_concentration",
  "Leaf phosphorus content per leaf dry mass" = "leaf_P_concentration",
  "Leaf carbon content per leaf dry mass" = "leaf_C_concentration",
  "Leaf mass per area" = "SLA"
)
