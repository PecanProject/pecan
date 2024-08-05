#' SDA　Downscale Function for Hourly Data
#' 
#' This function uses the randomForest model to downscale forecast data (hourly) to unmodeled locations using covariates and site locations
#' 
#' @author Harunobu Ishii
#' @param nc_file  In quotes, file path for .nc containing ensemble data.
#' @param coords In quotes, file path for .csv file containing the site coordinates, columns named "lon" and "lat".
#' @param yyyy In string, format is yyyy(year of interest)
#' @param covariates SpatRaster stack, used as predictors in randomForest. Layers within stack should be named. Recommended that this stack be generated using 'covariates' instructions in assim.sequential/inst folder
#' @return It returns the `downscale_output` list containing lists for the training and testing data sets, models, and predicted maps for each ensemble member.
#' @export

SDA_downscale_hrly <- function(nc_file, coords, yyyy, covariates){
  
  # Read the input data and site coordinates
  nc_data <- ncdf4::nc_open(nc_file)
  on.exit(ncdf4::nc_close(nc_data))
  input_data <- ncdf4::ncvar_get(nc_data, "NEE")
  covariate_names <- names(covariates)
  
  
  # Extract time and units
  time <- nc_data$dim$time$vals
  time_units <- nc_data$dim$time$units
  time_origin_str <- substr(time_units, 12, 31)
  
  # Check if timezone is specified in the time units string
  if (grepl("UTC|GMT", time_units)) {
    time_origin <- lubridate::ymd_hm(time_origin_str, tz = "UTC")
  } else if (grepl("EST", time_units)) {
    time_origin <- lubridate::ymd_hm(time_origin_str, tz = "EST")
  } else {
    time_origin <- lubridate::ymd_hm(time_origin_str, tz = "UTC")  # Default to UTC if not specified
  }
  
  # Timereadable
  if (grepl("hours", time_units)) {
    time_readable <- time_origin + lubridate::dhours(time)
  } else if (grepl("seconds", time_units)) {
    time_readable <- time_origin + lubridate::dseconds(time)
  } else {
    stop("Unsupported time units")
  }
  
  # Extract predictors from covariates raster using site coordinates
  site_coordinates <- terra::vect(readr::read_csv(coords), geom=c("lon", "lat"), crs="EPSG:4326")
  predictors <- as.data.frame(terra::extract(covariates, site_coordinates,ID = FALSE)) 

  downscale_output<- list()
  
  # Train & Test split
  sample <- sample(1:nrow(predictors), size = round(0.75*nrow(predictors)))
  
  # Predict for each time stamp of the year selected
  time_indices <- which(year(time_readable) == yyyy)
  for (index in time_indices) {
    data <- input_data[index, , ]
    carbon_data <- as.data.frame(data)
    names(carbon_data) <- paste0("ensemble",seq(1:ncol(carbon_data)))

    # Combine carbon data and covariates/predictors and split into training/test
    full_data <- cbind(carbon_data, predictors)
    train_data <- full_data[sample, ]
    test_data <- full_data[-sample, ]
    
    # Combine each ensemble member with all predictors
    models <- list()
    maps <- list()
    predictions <- list()
    ensembles <- list()
    for (i in seq_along(carbon_data)) {
      ensemble_col <- paste0("ensemble", i)
      formula <- stats::as.formula(paste(ensemble_col, "~", paste(covariate_names, collapse = " + ")))
      models[[i]] <- randomForest::randomForest(formula,
                                                data = train_data,
                                                ntree = 1000,
                                                na.action = stats::na.omit,
                                                keep.forest = TRUE,
                                                importance = TRUE)
      
      maps[[i]] <- terra::predict(covariates, model = models[[i]], na.rm = TRUE)
      predictions[[i]] <- stats::predict(models[[i]], test_data)
    }

    # Organize the results into a single output list
    curr_downscaled <- list( data = list(training = train_data, testing = test_data),
                             models = models,
                             maps = maps,
                             predictions = predictions
                            )
    
    # Rename each element of the output list with appropriate ensemble numbers
    for (i in 1:length(curr_downscaled$data)) {
      names(curr_downscaled$data[[i]]) <- paste0("ensemble", seq(1:ncol(carbon_data)))
    }
    names(curr_downscaled$models) <- paste0("ensemble", seq(1:ncol(carbon_data)))
    names(curr_downscaled$maps) <- paste0("ensemble", seq(1:ncol(carbon_data)))
    names(curr_downscaled$predictions) <- paste0("ensemble", seq(1:ncol(carbon_data)))
    
    downscale_output[[as.character(time_readable[index])]]<-curr_downscaled
  }
  return(downscale_output)
}
