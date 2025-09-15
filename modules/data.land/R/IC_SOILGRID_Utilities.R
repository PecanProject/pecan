#' SoilGrids Initial Conditions (IC) Utilities
#' @description Functions for generating soil carbon IC files from SoilGrids250m data
#' @details This module provides functions for extracting, processing, and generating
#'          ensemble members for soil carbon initial conditions using SoilGrids data.
#'          All soil carbon values are in kg/m2.
#'
#' Process SoilGrids data for initial conditions
#' 
#' @param settings PEcAn settings list containing site information
#' @param dir Output directory for IC files
#' @param depth Numeric vector of depth values in meters. Can be single value
#'              or multiple values c(0.3, 2.0). Default: c(0.3, 2.0)
#' @param overwrite Overwrite existing files? (Default: FALSE)
#' @param verbose Print detailed progress information? (Default: FALSE)
#' 
#' @return List of paths to generated IC files, organized by site ID
#' @export
#'
#' @examples
#' \dontrun{
#' # Process both depths (default)
#' settings <- PEcAn.settings::read.settings("pecan.xml")
#' output_dir <- withr::local_tempdir()
#' ic_files <- soilgrids_ic_process(settings, dir = output_dir)  
#' 
#' # Process only 30cm depth
#' ic_files <- soilgrids_ic_process(settings, dir = output_dir, depth = 0.3)
#' }
#'
#' @author Akash
#'
soilgrids_ic_process <- function(settings, dir, depth = c(0.3, 2.0), overwrite = FALSE, verbose = FALSE) {
  start_time <- proc.time()
  
  valid_depths <- c(0.3, 2.0)
  if (!all(depth %in% valid_depths)) {
    PEcAn.logger::logger.severe(sprintf("Invalid depth values. Must be from: %s", 
                                        paste(valid_depths, collapse = ", ")))
  }
  depth <- sort(unique(depth))
  depth_layers <- sapply(depth, function(d) if (d == 0.3) "0-30cm" else "0-200cm")
  
  if (verbose) {
    PEcAn.logger::logger.info(sprintf("Processing soil carbon data for depths: %s", 
                                      paste(paste0(depth, "m (", depth_layers, ")"), collapse = ", ")))
  }
  
  site_info <- settings$run$site
  if (is.list(site_info) && !is.null(site_info$id)) {
    site_info <- list(site_info)
  }
  site_info <- site_info |>
    purrr::map(function(site) {
      site$lat <- as.numeric(site$lat)
      site$lon <- as.numeric(site$lon)
      data.frame(
        site_id = site$id,
        lat = site$lat,
        lon = site$lon,
        site_name = site$name,
        str_id = as.character(site$id),
        stringsAsFactors = FALSE
      )
    }) |>
    dplyr::bind_rows()
  n_sites <- nrow(site_info)
  if (n_sites == 0) {
    PEcAn.logger::logger.severe("No sites found in the provided input")
  }
  
  size <- ifelse(is.null(settings$ensemble$size), 1, settings$ensemble$size)
  
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
  
  data_dir <- file.path(dir, "SoilGrids_data")
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE)
  }
  
  # Check for cached data
  soilc_csv_path <- file.path(data_dir, "soilgrids_soilC_data.csv")
  if (file.exists(soilc_csv_path) && !overwrite) {
    soil_data <- utils::read.csv(soilc_csv_path, check.names = FALSE)
  } else {
    soil_data <- PEcAn.data.land::soilgrids_soilC_extract(
      site_info = site_info,
      outdir = data_dir,
      verbose = verbose
    )
    # Save the extracted data for future use
    utils::write.csv(soil_data, soilc_csv_path, row.names = FALSE)
  }
  
  # Validate soil carbon data units through range check for selected depths
  for (i in seq_along(depth_layers)) {
    depth_col <- paste0("Total_soilC_", depth_layers[i])
    if (any(soil_data[[depth_col]] > 150, na.rm = TRUE)) {
      PEcAn.logger::logger.warn(sprintf("Some soil carbon values exceed 150 kg/m2 for %s, values may be in wrong units", 
                                        depth_layers[i]))
    }
  }
  
  processed_data <- preprocess_soilgrids_data(soil_data, depth_layers, verbose)
  
  if (nrow(processed_data$data) == 0) {
    PEcAn.logger::logger.severe("No valid sites remain after preprocessing")
  }
  
  ens_files <- list()
  
  for (s in 1:nrow(processed_data$data)) {
    site_data <- processed_data$data[s, ]
    
    site_idx <- which(site_info$site_id == site_data$Site_ID)
    if (length(site_idx) == 0) {
      PEcAn.logger::logger.warn(sprintf("Site %s not found in site_info", site_data$Site_ID))
      next
    }
    current_site <- site_info[site_idx, ]
    
    # Create output directory for this site
    site_folder <- file.path(dir, paste0("SoilGrids_site_", current_site$str_id))
    if (!dir.exists(site_folder)) {
      dir.create(site_folder, recursive = TRUE)
    }
    
    # Check for existing files
    existing_files <- list.files(site_folder, "*.nc$", full.names = TRUE)
    if (length(existing_files) > 0 && !overwrite) {
      ens_files[[current_site$str_id]] <- existing_files
      next
    }
    
    # Generate ensemble members for each requested depth
    ens_data <- list()
    for (i in seq_along(depth_layers)) {
      ens_data[[i]] <- generate_soilgrids_ensemble(
        processed_data = processed_data,
        site_id = current_site$site_id,
        size = size,
        depth_layer = depth_layers[i],
        verbose = verbose
      )
    }
    
    site_files <- list()
    
    # Write each ensemble member to NetCDF files
    for (ens in seq_len(size)) {
      soil_c_values <- numeric(length(depth_layers))
      for (i in seq_along(depth_layers)) {
        soil_c_values[i] <- ens_data[[i]][ens]
      }
      
      ens_input <- list(
        dims = list(
          lat = current_site$lat,
          lon = current_site$lon,
          time = 1,
          depth = depth
        ),
        vals = list(
          soil_organic_carbon_content = soil_c_values
        )
      )
      result <- PEcAn.data.land::pool_ic_list2netcdf(
        input = ens_input,
        outdir = site_folder,
        siteid = current_site$site_id,
        ens = ens
      )
      
      site_files[[ens]] <- result$file
    }
    
    ens_files[[current_site$str_id]] <- site_files
  }
  
  if (verbose) {
    end_time <- proc.time()
    elapsed_time <- end_time - start_time
    PEcAn.logger::logger.info(sprintf("IC generation completed for %d site(s) in %.2f seconds", 
                                      n_sites, elapsed_time[3]))
  }
  
  return(ens_files)
}

#' Preprocess SoilGrids data for ensemble generation
#'
#' @param soil_data Dataframe with SoilGrids soil carbon data
#' @param depth_layers Character vector of depth layers to process (e.g., c("0-30cm", "0-200cm"))
#' @param verbose Logical, print detailed progress information
#' 
#' @return List containing processed data and CV distributions for requested depths
#' @export
preprocess_soilgrids_data <- function(soil_data, depth_layers, verbose = FALSE) {
  if (!requireNamespace("MASS", quietly = TRUE)) {
    PEcAn.logger::logger.severe("MASS package required for SoilGrids ensemble generation")
  }
  if (verbose) {
    PEcAn.logger::logger.info(sprintf("Preprocessing soil carbon data for depths: %s", 
                                      paste(depth_layers, collapse = ", ")))
  }
  
  mean_cols <- paste0("Total_soilC_", depth_layers)
  std_cols <- paste0("Std_soilC_", depth_layers)
  
  complete_sites <- rep(TRUE, nrow(soil_data))
  for (col in mean_cols) {
    complete_sites <- complete_sites & !is.na(soil_data[[col]]) & soil_data[[col]] > 0
  }
  
  if (!any(complete_sites)) {
    PEcAn.logger::logger.severe(sprintf("No sites with complete data for all requested depth intervals: %s", 
                                        paste(depth_layers, collapse = ", ")))
  }
  
  processed <- soil_data[complete_sites, ]
  
  if (verbose) {
    removed_count <- nrow(soil_data) - nrow(processed)
    PEcAn.logger::logger.info(sprintf("Removed %d site(s) with incomplete data. Processing %d sites for depths: %s", 
                                      removed_count, nrow(processed), paste(depth_layers, collapse = ", ")))
  }
  
  # Calculate CV distributions for each requested depth
  cv_distributions <- list()
  for (i in seq_along(depth_layers)) {
    mean_col <- mean_cols[i]
    std_col <- std_cols[i]
    
    valid_cv <- processed[[mean_col]] > 0 & 
      !is.na(processed[[std_col]]) & 
      processed[[std_col]] > 0
    
    if (sum(valid_cv) < 5) {
      cv_distributions[[depth_layers[i]]] <- list(type = "none")
    } else {
      cv_values <- processed[[std_col]][valid_cv] / processed[[mean_col]][valid_cv]
      cv_valid <- cv_values[cv_values > 0 & is.finite(cv_values)]    
      
      if (length(cv_valid) < 5) {
        cv_distributions[[depth_layers[i]]] <- list(type = "none")
      } else {
        gamma_fit <- try(MASS::fitdistr(cv_valid, "gamma"), silent = TRUE)
        if (!inherits(gamma_fit, "try-error")) {
          cv_distributions[[depth_layers[i]]] <- list(
            type = "gamma",
            shape = gamma_fit$estimate["shape"],
            rate = gamma_fit$estimate["rate"]
          )
        } else {
          cv_distributions[[depth_layers[i]]] <- list(
            type = "empirical",
            values = cv_valid
          )
        }
      }
    }
  }
  
  return(list(
    data = processed,
    cv_distributions = cv_distributions,
    depth_layers = depth_layers
  ))
}

#' Generate soil carbon ensemble members for specific depth
#'
#' @description Generates ensemble members for soil carbon at specified depth layer.
#' Uses site-specific uncertainty when available; otherwise integrates over coefficient of
#' variation distributions fit to population data. Samples are drawn from gamma distributions
#' to ensure positive, right-skewed values appropriate for soil carbon estimates.
#'
#' @param processed_data Output from preprocess_soilgrids_data()
#' @param site_id Target site ID
#' @param size Number of ensemble members to generate
#' @param depth_layer Depth layer ("0-30cm" or "0-200cm")
#' @param verbose Logical, print detailed progress information
#' 
#' @return Numeric vector of soil carbon values including uncertainty, length equal to size.
#' @export
generate_soilgrids_ensemble <- function(processed_data, site_id, size, depth_layer, verbose = FALSE) {
  if (verbose) {
    PEcAn.logger::logger.info(sprintf("Generating %d ensemble members for site %s (%s)",size, site_id, depth_layer))
  }
  
  site_row <- which(processed_data$data$Site_ID == site_id)
  if (length(site_row) == 0) {
    PEcAn.logger::logger.severe(sprintf("Site %s not found in processed data", site_id))
  }
  mean_col <- paste0("Total_soilC_", depth_layer)
  std_col <- paste0("Std_soilC_", depth_layer)
  
  mean_c <- processed_data$data[[mean_col]][site_row]
  original_sd <- processed_data$data[[std_col]][site_row]
  cv_dist <- processed_data$cv_distributions[[depth_layer]]
  
  if (is.na(mean_c) || mean_c <= 0) {
    PEcAn.logger::logger.severe(sprintf("Invalid mean soil carbon value for site %s (%s)", 
                                        site_id, depth_layer))
  }

  # Use site-specific uncertainty
  if (!is.na(original_sd) && original_sd > 0) {
    shape <- (mean_c^2) / (original_sd^2)
    rate <- mean_c / (original_sd^2)
    if (is.finite(shape) && is.finite(rate) && shape > 0 && rate > 0) {
      soil_c_values <- pmax(stats::rgamma(size, shape, rate), 0)
    } else {
      PEcAn.logger::logger.severe("Cannot generate an ensemble, invalid gamma params")
    }
  } else if (cv_dist$type != "none") {
    # Integrate over uncertainty using CV distribution
    if (cv_dist$type == "gamma") {
      cv_samples <- stats::rgamma(size, cv_dist$shape, cv_dist$rate)
    } else {
      cv_samples <- sample(cv_dist$values, size, replace = TRUE)
    }
    
    sd_values <- mean_c * cv_samples
    valid <- !is.na(sd_values) & sd_values > 0
    
    if (any(valid)) {
      soil_c_values <- numeric(size) # pre-allocate since we're doing partial assignment
      shape_vec <- (mean_c^2) / (sd_values[valid]^2)
      rate_vec <- mean_c / (sd_values[valid]^2)
      
      if (any(!is.finite(shape_vec)) || any(!is.finite(rate_vec)) || any(shape_vec <= 0) || any(rate_vec <= 0)) {
        PEcAn.logger::logger.severe("Cannot generate an ensemble, invalid gamma params")
      }
      
      soil_c_values[valid] <- pmax(stats::rgamma(sum(valid), shape_vec, rate_vec), 0)
      soil_c_values[!valid] <- NA
    } else {
      PEcAn.logger::logger.severe(sprintf("No valid sd_values to generate ensemble for site %s (%s)",
                                          site_id, depth_layer))
    }
  } else {
    PEcAn.logger::logger.severe(sprintf("No uncertainty information available for ensemble generation at site %s (%s)",
                                        site_id, depth_layer))
  }
  
  if (verbose) {
    PEcAn.logger::logger.debug(sprintf("Generated ensemble for site %s (%s): mean=%.2f, sd=%.2f",
                                       site_id, depth_layer, mean(soil_c_values), stats::sd(soil_c_values)
    ))
  }
  
  return(soil_c_values)
}