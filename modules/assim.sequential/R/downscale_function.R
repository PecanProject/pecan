##' @title North America Downscale Function
##' @name NA_downscale
##' @author Joshua Ploshay
##'
##' @param data  In quotes, file path for .rds containing ensemble data.
##' @param coords In quotes, file path for .csv file containing the site coordinates, columns named "lon" and "lat".
##' @param date In quotes, if SDA site run, format is yyyy/mm/dd, if NEON, yyyy-mm-dd. Restricted to years within file supplied to 'data'.
##' @param C_pool In quotes, carbon pool of interest. Name must match carbon pool name found within file supplied to 'data'.
##' @param covariates SpatRaster stack, used as predictors in randomForest. Layers within stack should be named. Recommended that this stack be generated using 'covariates' instructions in assim.sequential/inst folder
##' @details This function will downscale forecast data to unmodeled locations using covariates and site locations
##'
##' @description This function uses the randomForest model.
##'
##' @return It returns the `downscale_output` list containing lists for the training and testing data sets, models, and predicted maps for each ensemble member.


NA_downscale <- function(data, coords, date, C_pool, covariates){
  
  # Read the input data and site coordinates
  input_data <- readRDS(data)
  site_coordinates <- terra::vect(readr::read_csv(coords), geom=c("lon", "lat"), crs="EPSG:4326")
  
  # Extract the carbon data for the specified focus year
  index <- which(names(input_data) == date)
  data <- input_data[[index]]
  carbon_data <- as.data.frame(t(data[which(names(data) == C_pool)]))
  names(carbon_data) <- paste0("ensemble",seq(1:ncol(carbon_data)))
  
  # Extract predictors from covariates raster using site coordinates
  predictors <- as.data.frame(terra::extract(covariates, site_coordinates,ID = FALSE)) 
  
  # Combine each ensemble member with all predictors
  ensembles <- list()
  for (i in seq_along(carbon_data)) {
    ensembles[[i]] <- cbind(carbon_data[[i]], predictors)
  }
  
  # Rename the carbon_data column for each ensemble member
  for (i in 1:length(ensembles)) {
    ensembles[[i]] <- dplyr::rename(ensembles[[i]], "carbon_data" = "carbon_data[[i]]")
  }
  
  # Split the observations in each data frame into two data frames based on the proportion of 3/4
  ensembles <- lapply(ensembles, function(df) {
    sample <- sample(1:nrow(df), size = round(0.75*nrow(df)))
    train  <- df[sample, ]
    test   <- df[-sample, ]
    split_list <- list(train, test)
    return(split_list)
  })
  
  # Rename the training and testing data frames for each ensemble member
  for (i in 1:length(ensembles)) {
    # names(ensembles) <- paste0("ensemble",seq(1:length(ensembles)))
    names(ensembles[[i]]) <- c("training", "testing")
  }
  
  # Train a random forest model for each ensemble member using the training data
  rf_output <- list()
  for (i in 1:length(ensembles)) {
    rf_output[[i]] <- randomForest::randomForest(ensembles[[i]][[1]][["carbon_data"]] ~ land_cover+tavg+prec+srad+vapr+nitrogen+phh2o+soc+sand,
                                                 data = ensembles[[i]][[1]],
                                                 ntree = 1000,
                                                 na.action = stats::na.omit,
                                                 keep.forest = T,
                                                 importance = T)
  }
  
  # Generate predictions (maps) for each ensemble member using the trained models
  maps <- list(ncol(rf_output))
  for (i in 1:length(rf_output)) {
    maps[[i]] <- terra::predict(object = covariates,
                                model = rf_output[[i]],na.rm = T)
  }
  
  # Organize the results into a single output list
  downscale_output <- list(ensembles, rf_output, maps)
  
  # Rename each element of the output list with appropriate ensemble numbers
  for (i in 1:length(downscale_output)) {
    names(downscale_output[[i]]) <- paste0("ensemble",seq(1:length(downscale_output[[i]])))
  }
  
  # Rename the main components of the output list
  names(downscale_output) <- c("data", "models", "maps")
  
  return(downscale_output)
}
