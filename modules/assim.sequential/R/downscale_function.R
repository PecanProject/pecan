##' @title Preprocess Data for Downscaling
##' @name SDA_downscale_preprocess
##' @author Sambhav Dixit
##'
##' @param data_path Character. File path for .rds containing ensemble data.
##' @param coords_path Character. File path for .csv file containing the site coordinates, with columns named "lon" and "lat".
##' @param date Character. If SDA site run, format is yyyy/mm/dd; if NEON, yyyy-mm-dd. Restricted to years within the file supplied to 'data_path'.
##' @param carbon_pool Character. Carbon pool of interest. Name must match the carbon pool name found within the file supplied to 'data_path'.
##' @details This function ensures that the specified date and carbon pool are present in the input data. It also checks the validity of the site coordinates and aligns the number of rows between site coordinates and carbon data.
##'
##' @description This function reads and checks the input data, ensuring that the required date and carbon pool exist, and that the site coordinates are valid.
##'
##' @return A list containing The read .rds data , The cleaned site coordinates, and the preprocessed carbon data.

# Preprocess function to check and clean the data
SDA_downscale_preprocess <- function(data_path, coords_path, date, carbon_pool) {
  # Read the input data and site coordinates
  input_data <- readRDS(data_path)
  site_coordinates <- readr::read_csv(coords_path)
  
  # Convert input_data names to standard date format
  input_date_names <- suppressWarnings(as.character(lubridate::ymd(names(input_data))))
  names(input_data) <- ifelse(is.na(input_date_names), 
                              names(input_data),
                              input_date_names)
  
  # Convert the input date to standard format
  standard_date <- as.character(lubridate::ymd(date))
  
  # Ensure the date exists in the input data
  if (!standard_date %in% names(input_data)) {
    stop(paste("Date", date, "not found in the input data."))
  }
  
  # Extract the carbon data for the specified focus year
  index <- which(names(input_data) == standard_date)
  data <- input_data[[index]]
  
  # Rest of the function remains the same...
  
  # Ensure the carbon pool exists in the input data
  if (!carbon_pool %in% names(data)) {
    stop(paste("Carbon pool", carbon_pool, "not found in the input data."))
  }
  
  carbon_data <- as.data.frame(t(data[which(names(data) == carbon_pool)]))
  names(carbon_data) <- paste0("ensemble", seq(ncol(carbon_data)))
  
  # Ensure site coordinates have 'lon' and 'lat' columns
  if (!all(c("lon", "lat") %in% names(site_coordinates))) {
    stop("Site coordinates must contain 'lon' and 'lat' columns.")
  }
  
  # Ensure the number of rows in site coordinates matches the number of rows in carbon data
  if (nrow(site_coordinates) != nrow(carbon_data)) {
    message("Number of rows in site coordinates does not match the number of rows in carbon data.")
    if (nrow(site_coordinates) > nrow(carbon_data)) {
      message("Truncating site coordinates to match carbon data rows.")
      site_coordinates <- site_coordinates[1:nrow(carbon_data), ]
    } else {
      message("Truncating carbon data to match site coordinates rows.")
      carbon_data <- carbon_data[1:nrow(site_coordinates), ]
    }
  }
  
  message("Preprocessing completed successfully.")
  return(list(input_data = input_data, site_coordinates = site_coordinates, carbon_data = carbon_data))
}

##' @title North America Downscale Function
##' @name SDA_downscale
##' @author Joshua Ploshay , Sambhav Dixit
##'
##' @param preprocessed , In quotes, prepocessed data returned as an output for passing the raw data to the NA_preprocess function.
##' @param date In quotes, if SDA site run, format is yyyy/mm/dd, if NEON, yyyy-mm-dd. Restricted to years within file supplied to 'data'.
##' @param carbon_pool In quotes, carbon pool of interest. Name must match carbon pool name found within file supplied to 'data'.
##' @param covariates SpatRaster stack, used as predictors in CNN. Layers within stack should be named. Recommended that this stack be generated using 'covariates' instructions in assim.sequential/inst folder
##' @details This function will downscale forecast data to unmodeled locations using covariates and site locations
##'
##' @description This function uses the Convolutional Neural Network(CNN) model.
##'
##' @return It returns the `downscale_output` list containing lists for the training and testing data sets, models, and predicted maps for each ensemble member.


SDA_downscale <- function(preprocessed, date, carbon_pool, covariates, model_type = "rf") {
  input_data <- preprocessed$input_data
  site_coordinates <- preprocessed$site_coordinates
  carbon_data <- preprocessed$carbon_data
  
  # Convert site coordinates to SpatVector
  site_coordinates <- terra::vect(site_coordinates, geom = c("lon", "lat"), crs = "EPSG:4326")
  
  # Extract predictors from covariates raster using site coordinates
  predictors <- as.data.frame(terra::extract(covariates, site_coordinates, ID = FALSE))
  
  # Combine each ensemble member with all predictors
  ensembles <- list()
  for (i in seq_along(carbon_data)) {
    ensembles[[i]] <- cbind(carbon_data[[i]], predictors)
  }
  
  if (model_type == "rf") {
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
      names(ensembles[[i]]) <- c("training", "testing")
    }
    
    # Train a random forest model for each ensemble member using the training data
    rf_output <- list()
    for (i in 1:length(ensembles)) {
      rf_output[[i]] <- randomForest::randomForest(ensembles[[i]][[1]][["carbon_data"]] ~ tavg+prec+srad+vapr,
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
                                  model = rf_output[[i]], na.rm = T)
    }
    
    # Organize the results into a single output list
    downscale_output <- list(ensembles, rf_output, maps)
    
    # Rename each element of the output list with appropriate ensemble numbers
    for (i in 1:length(downscale_output)) {
      names(downscale_output[[i]]) <- paste0("ensemble", seq(1:length(downscale_output[[i]])))
    }
    
    # Rename the main components of the output list
    names(downscale_output) <- c("data", "models", "maps")
    
  } else if (model_type == "cnn") {
    # Rename the carbon_data column for each ensemble member
    for (i in 1:length(ensembles)) {
      colnames(ensembles[[i]])[1] <- paste0(carbon_pool, "_ens", i)
    }
    
    # Split the observations in each data frame into two data frames based on the proportion of 3/4
    ensembles <- lapply(ensembles, function(df) {
      sample <- sample(1:nrow(df), size = round(0.75 * nrow(df)))
      train <- df[sample, ]
      test <- df[-sample, ]
      split_list <- list(training = train, testing = test)
      return(split_list)
    })
    
    # Train a CNN model for each ensemble member using the training data
    cnn_output <- list()
    scaling_params <- list()
    for (i in 1:length(ensembles)) {
      # Prepare data for CNN
      x_train <- as.matrix(ensembles[[i]]$training[, c("tavg", "prec", "srad", "vapr")])
      y_train <- as.matrix(ensembles[[i]]$training[[paste0(carbon_pool, "_ens", i)]])
      x_test <- as.matrix(ensembles[[i]]$testing[, c("tavg", "prec", "srad", "vapr")])
      y_test <- as.matrix(ensembles[[i]]$testing[[paste0(carbon_pool, "_ens", i)]])
      
      # Calculate scaling parameters from training data
      scaling_params[[i]] <- list(
        mean = colMeans(x_train),
        sd = apply(x_train, 2, sd)
      )
      
      # Normalize the data using training data parameters
      x_train <- scale(x_train, center = scaling_params[[i]]$mean, scale = scaling_params[[i]]$sd)
      x_test <- scale(x_test, center = scaling_params[[i]]$mean, scale = scaling_params[[i]]$sd)
      
      # Reshape data for CNN input (samples, timesteps, features)
      x_train <- array_reshape(x_train, c(nrow(x_train), 1, ncol(x_train)))
      x_test <- array_reshape(x_test, c(nrow(x_test), 1, ncol(x_test)))
      
      # Define the CNN model
      model <- keras_model_sequential() |>
        layer_conv_1d(filters = 64, kernel_size = 1, activation = 'relu', input_shape = c(1, 4)) |>
        layer_flatten() |>
        layer_dense(units = 64, activation = 'relu') |>
        layer_dense(units = 1)
      
      # Compile the model
      model |> compile(
        loss = 'mean_squared_error',
        optimizer = optimizer_adam(),
        metrics = c('mean_absolute_error')
      )
      
      # Train the model
      model |> fit(
        x = x_train,
        y = y_train,
        epochs = 100,
        batch_size = 32,
        validation_split = 0.2,
        verbose = 0
      )
      
      cnn_output[[i]] <- model
    }
    
    # Wrapper function to apply the trained model
    predict_with_model <- function(model, data, scaling_params) {
      data <- as.matrix(data[, c("tavg", "prec", "srad", "vapr")])
      data <- scale(data, center = scaling_params$mean, scale = scaling_params$sd)
      data <- array_reshape(data, c(nrow(data), 1, ncol(data)))
      predictions <- predict(model, data)
      return(predictions)
    }
    
    # Generate predictions (maps) for each ensemble member using the trained models
    maps <- list()
    predictions <- list()
    for (i in 1:length(cnn_output)) {
      map_pred <- predict_with_model(cnn_output[[i]], as.data.frame(covariates[]), scaling_params[[i]])
      map_pred <- terra::rast(matrix(map_pred, nrow = nrow(covariates), ncol = ncol(covariates)), ext = terra::ext(covariates), crs = terra::crs(covariates))
      maps[[i]] <- map_pred
      
      # Generate predictions for testing data
      predictions[[i]] <- predict_with_model(cnn_output[[i]], ensembles[[i]]$testing, scaling_params[[i]])
    }
    
    # Calculate performance metrics for each ensemble member
    metrics <- list()
    for (i in 1:length(predictions)) {
      actual <- ensembles[[i]]$testing[[paste0(carbon_pool, "_ens", i)]]
      predicted <- predictions[[i]]
      mse <- mean((actual - predicted)^2)
      mae <- mean(abs(actual - predicted))
      r_squared <- 1 - sum((actual - predicted)^2) / sum((actual - mean(actual))^2)
      metrics[[i]] <- list(MSE = mse, MAE = mae, R_squared = r_squared, actual = actual, predicted = predicted)
    }
    
    # Organize the results into a single output list
    downscale_output <- list(data = ensembles, models = cnn_output, maps = maps, metrics = metrics, scaling_params = scaling_params)
    
    # Rename each element of the output list with appropriate ensemble numbers
    for (i in 1:length(downscale_output$data)) {
      names(downscale_output$data)[i] <- paste0("ensemble", i)
      names(downscale_output$models)[i] <- paste0("ensemble", i)
      names(downscale_output$maps)[i] <- paste0("ensemble", i)
      names(downscale_output$metrics)[i] <- paste0("ensemble", i)
      names(downscale_output$scaling_params)[i] <- paste0("ensemble", i)
    }
    
  } else {
    stop("Invalid model_type. Please choose either 'rf' for Random Forest or 'cnn' for Convolutional Neural Network.")
  }
  
  return(downscale_output)
}
