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
  input_data <- base::readRDS(data_path)
  site_coordinates <- readr::read_csv(coords_path)
  
  # Convert input_data names to Date objects
  input_date_names <- lubridate::ymd(base::names(input_data))
  base::names(input_data) <- input_date_names
  
  # Convert the input date to a Date object
  standard_date <- lubridate::ymd(date)
  
  # Ensure the date exists in the input data
  if (!standard_date %in% input_date_names) {
    base::stop(base::paste("Date", date, "not found in the input data."))
  }
  
  # Extract the carbon data for the specified focus year
  index <- base::which(input_date_names == standard_date)
  data <- input_data[[index]]
  
  # Ensure the carbon pool exists in the input data
  if (!carbon_pool %in% base::names(data)) {
    base::stop(base::paste("Carbon pool", carbon_pool, "not found in the input data."))
  }
  
  carbon_data <- base::as.data.frame(base::t(data[base::which(base::names(data) == carbon_pool)]))
  base::names(carbon_data) <- base::paste0("ensemble", base::seq(base::ncol(carbon_data)))
  
  # Ensure site coordinates have 'lon' and 'lat' columns
  if (!base::all(c("lon", "lat") %in% base::names(site_coordinates))) {
    base::stop("Site coordinates must contain 'lon' and 'lat' columns.")
  }
  
  # Ensure the number of rows in site coordinates matches the number of rows in carbon data
  if (base::nrow(site_coordinates) != base::nrow(carbon_data)) {
    base::message("Number of rows in site coordinates does not match the number of rows in carbon data.")
    if (base::nrow(site_coordinates) > base::nrow(carbon_data)) {
      base::message("Truncating site coordinates to match carbon data rows.")
      site_coordinates <- site_coordinates[1:base::nrow(carbon_data), ]
    } else {
      base::message("Truncating carbon data to match site coordinates rows.")
      carbon_data <- carbon_data[1:base::nrow(site_coordinates), ]
    }
  }
  
  base::message("Preprocessing completed successfully.")
  base::return(base::list(input_data = input_data, site_coordinates = site_coordinates, carbon_data = carbon_data))
}

##' @title SDA Downscale Function
##' @name SDA_downscale
##' @author Joshua Ploshay, Sambhav Dixit
##'
##' @param preprocessed List. Preprocessed data returned as an output from the SDA_downscale_preprocess function.
##' @param date Date. If SDA site run, format is yyyy/mm/dd; if NEON, yyyy-mm-dd. Restricted to years within file supplied to 'preprocessed' from the 'data_path'.
##' @param carbon_pool Character. Carbon pool of interest. Name must match carbon pool name found within file supplied to 'preprocessed' from the 'data_path'.
##' @param covariates SpatRaster stack. Used as predictors in CNN. Layers within stack should be named.
##' @param model_type Character. Either "rf" for Random Forest or "cnn" for Convolutional Neural Network. Default is Random Forest.
##' @param seed Numeric or NULL. Optional seed for random number generation. Default is NULL.
##' @details This function will downscale forecast data to unmodeled locations using covariates and site locations
##'
##' @description This function uses either Random Forest or Convolutional Neural Network model based on the model_type parameter.
##'
##' @return A list containing the training and testing data sets, models, predicted maps for each ensemble member, and predictions for testing data.

SDA_downscale <- function(preprocessed, date, carbon_pool, covariates, model_type = "rf", seed = NULL) {
  input_data <- preprocessed$input_data
  site_coordinates <- preprocessed$site_coordinates
  carbon_data <- preprocessed$carbon_data
  
  # Convert site coordinates to SpatVector
  site_coordinates <- terra::vect(site_coordinates, geom = c("lon", "lat"), crs = "EPSG:4326")
  
  # Extract predictors from covariates raster using site coordinates
  predictors <- base::as.data.frame(terra::extract(covariates, site_coordinates, ID = FALSE))
  
  # Dynamically get covariate names
  covariate_names <- base::names(predictors)
  
  # Create a single data frame with all predictors and ensemble data
  full_data <- base::cbind(carbon_data, predictors)
  
  # Split the observations into training and testing sets
  if (!base::is.null(seed)) {
    base::set.seed(seed)  # Only set seed if provided
  }
  sample <- base::sample(1:base::nrow(full_data), size = base::round(0.75 * base::nrow(full_data)))
  train_data <- full_data[sample, ]
  test_data <- full_data[-sample, ]
  
  # Prepare data for both RF and CNN
  x_data <- base::as.matrix(full_data[, covariate_names])
  y_data <- base::as.matrix(carbon_data)
  
  # Calculate scaling parameters from all data
  scaling_params <- base::list(
    mean = base::colMeans(x_data),
    sd = base::apply(x_data, 2, stats::sd)
  )
  
  # Normalize the data
  x_data_scaled <- base::scale(x_data, center = scaling_params$mean, scale = scaling_params$sd)
  
  # Split into training and testing sets
  x_train <- x_data_scaled[sample, ]
  x_test <- x_data_scaled[-sample, ]
  y_train <- y_data[sample, ]
  y_test <- y_data[-sample, ]
  
  if (model_type == "rf") {
    # Train a random forest model for each ensemble member using the training data
    rf_output <- base::list()
    for (i in base::seq_along(carbon_data)) {
      ensemble_col <- base::paste0("ensemble", i)
      formula <- stats::as.formula(base::paste(ensemble_col, "~", base::paste(covariate_names, collapse = " + ")))
      rf_output[[i]] <- randomForest::randomForest(formula,
                                                   data = train_data,
                                                   ntree = 1000,
                                                   na.action = stats::na.omit,
                                                   keep.forest = TRUE,
                                                   importance = TRUE)
    }
    
    # Generate predictions (maps) for each ensemble member using the trained models
    maps <- base::list()
    predictions <- base::list()
    for (i in base::seq_along(rf_output)) {
      maps[[i]] <- terra::predict(covariates, model = rf_output[[i]], na.rm = TRUE)
      predictions[[i]] <- stats::predict(rf_output[[i]], test_data)
    }
    
    # Organize the results into a single output list
    downscale_output <- base::list(
      data = base::list(training = train_data, testing = test_data),
      models = rf_output,
      maps = maps,
      predictions = predictions
    )
    
  } else if (model_type == "cnn") {
    # Reshape data for CNN input (samples, timesteps, features)
    x_train <- array_reshape(x_train, c(base::nrow(x_train), 1, base::ncol(x_train)))
    x_test <- array_reshape(x_test, c(base::nrow(x_test), 1, base::ncol(x_test)))
    
    # Train a CNN model for each ensemble member
    cnn_output <- base::list()
    for (i in base::seq_along(carbon_data)) {
      # Define the CNN model
      model <- keras_model_sequential() |>
        layer_conv_1d(filters = 64, kernel_size = 1, activation = 'relu', input_shape = c(1, base::length(covariate_names))) |>
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
        y = y_train[, i],
        epochs = 100,
        batch_size = 32,
        validation_split = 0.2,
        verbose = 0
      )
      
      cnn_output[[i]] <- model
    }
    
    # Custom predict function for CNN
    cnn_predict <- function(model, newdata, scaling_params) {
      newdata <- base::scale(newdata, center = scaling_params$mean, scale = scaling_params$sd)
      newdata <- array_reshape(newdata, c(base::nrow(newdata), 1, base::ncol(newdata)))
      predictions <- stats::predict(model, newdata)
      base::return(base::as.vector(predictions))
    }
    
    # Generate predictions (maps) for each ensemble member using the trained models
    maps <- base::list()
    predictions <- base::list()
    for (i in base::seq_along(cnn_output)) {
      # Create a SpatRaster with the same properties as covariates
      prediction_rast <- terra::rast(covariates)
      
      # Use terra::predict to apply the CNN model
      maps[[i]] <- terra::predict(prediction_rast, model = cnn_output[[i]],
                                  fun = cnn_predict,
                                  scaling_params = scaling_params)
      
      # Generate predictions for testing data
      predictions[[i]] <- cnn_predict(cnn_output[[i]], x_data[-sample, ], scaling_params)
    }
    
    # Organize the results into a single output list
    downscale_output <- base::list(
      data = base::list(training = train_data, testing = test_data),
      models = cnn_output,
      maps = maps,
      predictions = predictions,
      scaling_params = scaling_params
    )
  } else {
    base::stop("Invalid model_type. Please choose either 'rf' for Random Forest or 'cnn' for Convolutional Neural Network.")
  }
  
  # Rename each element of the output list with appropriate ensemble numbers
  for (i in base::seq_along(carbon_data)) {
    base::names(downscale_output$models)[i] <- base::paste0("ensemble", i)
    base::names(downscale_output$maps)[i] <- base::paste0("ensemble", i)
    base::names(downscale_output$predictions)[i] <- base::paste0("ensemble", i)
  }
  
  base::return(downscale_output)
}

##' @title Calculate Metrics for Downscaling Results
##' @name SDA_downscale_metrics
##' @author Sambhav Dixit
##'
##' @param downscale_output List. Output from the SDA_downscale function, containing data, models, maps, and predictions for each ensemble.
##' @param carbon_pool Character. Name of the carbon pool used in the downscaling process.
##'
##' @details This function calculates performance metrics for the downscaling results. It computes Mean Squared Error (MSE), Mean Absolute Error (MAE), and R-squared for each ensemble. The function uses the actual values from the testing data and the predictions generated during the downscaling process.
##'
##' @description This function takes the output from the SDA_downscale function and computes various performance metrics for each ensemble. It provides a way to evaluate the accuracy of the downscaling results without modifying the main downscaling function.
##'
##' @return A list of metrics for each ensemble, where each element contains MAE , MSE ,R_squared ,actual values from testing data and predicted values for the testing data 

SDA_downscale_metrics <- function(downscale_output, carbon_pool) {
  metrics <- base::list()
  
  for (i in 1:base::length(downscale_output$data)) {
    actual <- downscale_output$data[[i]]$testing[[base::paste0(carbon_pool, "_ens", i)]]
    predicted <- downscale_output$predictions[[i]]
    
    mse <- base::mean((actual - predicted)^2)
    mae <- base::mean(base::abs(actual - predicted))
    r_squared <- 1 - base::sum((actual - predicted)^2) / base::sum((actual - base::mean(actual))^2)
    
    metrics[[i]] <- base::list(MSE = mse, MAE = mae, R_squared = r_squared, actual = actual, predicted = predicted)
  }
  
  base::names(metrics) <- base::paste0("ensemble", base::seq_along(metrics))
  
  base::return(metrics)
}
