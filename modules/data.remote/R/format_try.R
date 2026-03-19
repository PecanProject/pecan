#' Create a predefined trait mapping from TRY to PEcAn
#' 
#' @return A named character vector where names are TRY trait names and values are PEcAn variable names.
#' @export
try_trait_mapping <- function() {
  c(
    `Leaf carbon (C) content per leaf dry mass` = "leafC",
    `Leaf nitrogen (N) content per leaf dry mass` = "leafN",
    `Leaf phosphorus (P) content per leaf dry mass` = "leafP",
    `Specific leaf area (SLA) or specific leaf area (SLA)` = "SLA",
    `Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA)` = "SLA",
    `Leaf dry mass per leaf fresh mass (leaf dry matter content, LDMC)` = "LDMC",
    `Wood density (stem specific density, stem dry mass per stem fresh volume) or wood specific gravity` = "wood_density",
    `Plant height vegetative` = "plant_height",
    `Leaf assimilation rate (Amax)` = "Amax",
    `Leaf respiration rate in the dark per leaf area (dark respiration)` = "dark_respiration"
  )
}

#' Convert TRY database data to PEcAn format for meta-analysis
#' 
#' This function converts trait data from the external TRY database into the tabular 
#' format required by the PEcAn meta-analysis module. 
#' 
#' @details 
#' The resulting data frame is in the format returned by the PEcAn database (`PEcAn.DB::query.traits()`). 
#' To be used in `pecan.ma()`, **it must first be run through the `jagify()` function** 
#' to ensure all missing values and columns are appropriately processed, as the meta-analysis 
#' model requires data converted into specific distributions and error formats. 
#' `jagify` will drop unused TRY-specific columns and compute the final `obs.prec` values.
#' 
#' @param try_data A data frame containing data from the TRY database.
#' @param trait_map A named character vector for mapping TRY TraitName to PEcAn vname. 
#'   Names should be TRY TraitName and values should be PEcAn vname. 
#'   Defaults to `try_trait_mapping()`.
#' @param species_map An optional named vector mapping TRY `SpeciesName` to PEcAn BETY `specie_id`. 
#'   If provided, this is used to assign `specie_id` instead of using the raw `AccSpeciesID`. 
#'   If omitted, the returning data will include a `species_name` column to help users map PFTs later.
#' @return A data frame formatted similarly to BETYdb output to be passed to `PEcAn.MA::jagify`.
#' @export
format_try_for_ma <- function(try_data, trait_map = try_trait_mapping(), species_map = NULL) {
  # Ensure required columns are present. We do not explicitly filter out rows with missing TraitID 
  # so that covariates can be retained and mapped via trait_map if needed.
  data_filtered <- try_data
  if (!"TraitName" %in% names(data_filtered) || !("StdValue" %in% names(data_filtered))) {
    stop("Input data must contain 'TraitName' and 'StdValue' columns. Without these, this function cannot format actual traits!")
  }
  
  # Map TRY TraitName to PEcAn variable names using trait_map
  data_filtered$vname <- unname(trait_map[as.character(data_filtered$TraitName)])
  
  # Filter out unmapped traits and warn user about what was dropped
  unmapped_idx <- is.na(data_filtered$vname)
  if (any(unmapped_idx)) {
    unmapped_names <- unique(data_filtered$TraitName[unmapped_idx])
    warning(sprintf("The following TRY traits or covariates were not mapped and will be filtered out: %s", 
                    paste(unmapped_names, collapse = ", ")))
  }
  data_filtered <- data_filtered[!unmapped_idx, ]
  
  if (nrow(data_filtered) == 0) {
    stop("No traits were mapped successfully. Result is empty.")
  }

  mean_val <- as.numeric(data_filtered$StdValue)
  if (all(is.na(mean_val))) {
    stop("No actual numeric data found in the StdValue column!")
  }
  
  # Group site_id based on geospatial info if available
  if ("Latitude" %in% names(data_filtered) && "Longitude" %in% names(data_filtered)) {
    site_id_val <- as.integer(factor(paste(data_filtered$Latitude, data_filtered$Longitude, sep = "_")))
  } else {
    warning("No geospatial info (Latitude/Longitude) found in trying to parse site_id. Using ObservationID as grouping instead.")
    if ("ObservationID" %in% names(data_filtered)) {
      site_id_val <- as.integer(factor(data_filtered$ObservationID))
    } else {
      site_id_val <- 1:nrow(data_filtered)
    }
  }

  # Prepare specie_id
  if (!is.null(species_map) && "SpeciesName" %in% names(data_filtered)) {
    specie_id_val <- unname(species_map[as.character(data_filtered$SpeciesName)])
    unmapped_spp <- is.na(specie_id_val)
    if (any(unmapped_spp)) {
      warning(sprintf("Some species were not found in species_map and will be assigned NA specie_id: %s", 
                      paste(unique(data_filtered$SpeciesName[unmapped_spp]), collapse = ", ")))
    }
  } else if ("AccSpeciesID" %in% names(data_filtered)) {
    specie_id_val <- data_filtered$AccSpeciesID
  } else if ("SpeciesName" %in% names(data_filtered)) {
    specie_id_val <- as.integer(factor(data_filtered$SpeciesName))
  } else {
    warning("No specie_id (e.g., AccSpeciesID) found in the TRY data. This is really important to assign appropriately.")
    specie_id_val <- NA
  }

  # Attempt to determine greenhouse status
  greenhouse_val <- rep(0, nrow(data_filtered))
  for (col in c("DataName", "Dataset", "Reference", "SpeciesName", "CovariateName", "CovariateValue")) {
    if (col %in% names(data_filtered)) {
      is_gh <- grepl("greenhouse|chamber|pot\\b|indoor|controlled", data_filtered[[col]], ignore.case = TRUE)
      greenhouse_val[is_gh] <- 1
    }
  }
  if (any(greenhouse_val == 1)) {
    warning("Some data was estimated as greenhouse/controlled environment based on text matching. Please verify manually if critical.")
  } else {
    warning("No greenhouse/chamber data detected via text matching. All data defaulting to field (greenhouse = 0). Please verify if needed.")
  }
  
  res <- data.frame(
    id = if ("ObsDataID" %in% names(data_filtered)) data_filtered$ObsDataID else 1:nrow(data_filtered),
    citation_id = -9999,
    site_id = site_id_val,
    treatment_id = NA,
    name = "control",
    date = if ("Date" %in% names(data_filtered)) data_filtered$Date else NA,
    time = if ("Time" %in% names(data_filtered)) data_filtered$Time else NA,
    cultivar_id = NA,
    specie_id = specie_id_val,
    species_name = if ("SpeciesName" %in% names(data_filtered)) data_filtered$SpeciesName else NA,
    mean = mean_val,
    statname = if ("ErrorRisk" %in% names(data_filtered)) "SE" else NA,
    stat = if ("ErrorRisk" %in% names(data_filtered)) as.numeric(data_filtered$ErrorRisk) else as.numeric(NA),
    n = if ("Replicates" %in% names(data_filtered)) as.numeric(data_filtered$Replicates) else as.numeric(NA),
    vname = data_filtered$vname,
    month = if ("Date" %in% names(data_filtered)) as.numeric(format(as.Date(data_filtered$Date), "%m")) else NA,
    lon = if ("Longitude" %in% names(data_filtered)) data_filtered$Longitude else NA,
    lat = if ("Latitude" %in% names(data_filtered)) data_filtered$Latitude else NA,
    control = 1,
    greenhouse = greenhouse_val
  )
  
  if ("DatasetID" %in% names(data_filtered)) {
    res$citation_id <- ifelse(is.na(data_filtered$DatasetID), -9999, as.numeric(data_filtered$DatasetID))
  }
  if ("ObservationID" %in% names(data_filtered)) {
    res$treatment_id <- as.numeric(data_filtered$ObservationID)
  }
  
  # Handle specific edge cases (ranges, qualifiers) similar to jagify
  if (any(!is.na(res$stat) & res$stat <= 0)) {
    bad_stats_vnames <- unique(res$vname[!is.na(res$stat) & res$stat <= 0])
    warning(sprintf("Found implausible values of SE <= 0 for following traits: %s. Setting them to NA.", 
                    paste(bad_stats_vnames, collapse = ", ")))
    res$stat[!is.na(res$stat) & res$stat <= 0] <- NA
  }

  # Assign safe default values for missing data similar to jagify's transform.nas
  res$control[is.na(res$control)] <- 1
  res$site_id[is.na(res$site_id)] <- 0
  res$greenhouse[is.na(res$greenhouse)] <- 0
  res$n[is.na(res$n)] <- 1
  res$n[res$n == 1 & !is.na(res$stat)] <- 2
  
  res <- res[!is.na(res$mean), ]
  
  return(res)
}
