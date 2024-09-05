##' @title Preprocess Data for Downscaling
##' @name SDA_downscale_preprocess
##' @author Sambhav Dixit
##'
##' @param data_path Character. File path for .rds containing ensemble data.
##' @param coords_path Character. File path for .csv file containing the site coordinates, with columns named "lon" and "lat".
##' @param date Date. If SDA site run, format is yyyy/mm/dd; if NEON, yyyy-mm-dd. Restricted to years within the file supplied to 'data_path'.
##' @param carbon_pool Character. Carbon pool of interest. Name must match the carbon pool name found within the file supplied to 'data_path'.
##' @details This function ensures that the specified date and carbon pool are present in the input data. It also checks the validity of the site coordinates and aligns the number of rows between site coordinates and carbon data.
##'
##' @description This function reads and checks the input data, ensuring that the required date and carbon pool exist, and that the site coordinates are valid.
##'
##' @return A list containing The read .rds data , The cleaned site coordinates, and the preprocessed carbon data.

SDA_downscale_preprocess <- function(data_path, coords_path, date, carbon_pool) {
  # Read the input data and site coordinates
  input_data <- readRDS(data_path)
  site_coordinates <- readr::read_csv(coords_path)
  
  # Convert input_data names to Date objects
  input_date_names <- lubridate::ymd(names(input_data))
  names(input_data) <- input_date_names
  
  # Convert the input date to a Date object
  standard_date <- lubridate::ymd(date)
  
  # Ensure the date exists in the input data
  if (!standard_date %in% input_date_names) {
    stop(paste("Date", date, "not found in the input data."))
  }
  
  # Extract the carbon data for the specified focus year
  index <- which(input_date_names == standard_date)
  data <- input_data[[index]]
  
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

##' @noRd
##'
##' @title Create folds function
##' @name .create_folds
##' @author Sambhav Dixit
##'
##' @param y Vector. A vector of outcome data or indices.
##' @param k Numeric. The number of folds to create.
##' @param list Logical. If TRUE, returns a list of fold indices. If FALSE, returns a vector.
##' @param returnTrain Logical. If TRUE, returns indices for training sets. If FALSE, returns indices for test sets.
##' @details This function creates k-fold indices for cross-validation. It can return either training or test set indices, and the output can be in list or vector format.
##'
##' @description This function generates k-fold indices for cross-validation, allowing for flexible output formats.
##'
##' @return A list of k elements (if list = TRUE), each containing indices for a fold, or a vector of indices (if list = FALSE).

.create_folds <- function(y, k, list = TRUE, returnTrain = FALSE) {
  n <- length(y)
  indices <- seq_len(n)
  folds <- split(indices, cut(seq_len(n), breaks = k, labels = FALSE))
  
  if (!returnTrain) {
    folds <- folds  # Test indices are already what we want
  } else {
    folds <- lapply(folds, function(x) indices[-x])  # Return training indices
  }
  
  if (!list) {
    folds <- unlist(folds)
  }
  
  return(folds)
}

##' @title SDA Downscale Function
##' @name SDA_downscale
##' @author Joshua Ploshay, Sambhav Dixit
##'
##' @param preprocessed List. Preprocessed data returned as an output from the SDA_downscale_preprocess function.
##' @param date Date. If SDA site run, format is yyyy/mm/dd; if NEON, yyyy-mm-dd. Restricted to years within file supplied to 'preprocessed' from the 'data_path'.
##' @param carbon_pool Character. Carbon pool of interest. Name must match carbon pool name found within file supplied to 'preprocessed' from the 'data_path'.
##' @param covariates SpatRaster stack. Used as predictors in downscaling. Layers within stack should be named. Recommended that this stack be generated using 'covariates' instructions in assim.sequential/inst folder
##' @param model_type Character. Either "rf" for Random Forest or "cnn" for Convolutional Neural Network. Default is Random Forest.
##' @param seed Numeric or NULL. Optional seed for random number generation. Default is NULL.
##' @details This function will downscale forecast data to unmodeled locations using covariates and site locations
##'
##' @description This function uses either Random Forest or Convolutional Neural Network model based on the model_type parameter.
##'
##' @return A list containing the training and testing data sets, models, predicted maps for each ensemble member, and predictions for testing data.

SDA_downscale <- function(preprocessed, date, carbon_pool, covariates, model_type = "rf", seed = NULL) {
  carbon_data <- preprocessed$carbon_data
  
  # Convert site coordinates to SpatVector
  site_coordinates <- terra::vect(preprocessed$site_coordinates, geom = c("lon", "lat"), crs = "EPSG:4326")
  
  # Extract predictors from covariates raster using site coordinates
  predictors <- as.data.frame(terra::extract(covariates, site_coordinates, ID = FALSE))
  
  # Dynamically get covariate names
  covariate_names <- names(predictors)
  
  # Create a single data frame with all predictors and ensemble data
  full_data <- cbind(carbon_data, predictors)
  
  # Split the observations into training and testing sets
  if (!is.null(seed)) {
    set.seed(seed)  # Only set seed if provided
  }
  sample <- sample(1:nrow(full_data), size = round(0.75 * nrow(full_data)))
  train_data <- full_data[sample, ]
  test_data <- full_data[-sample, ]
  
  # Prepare data for both RF and CNN
  x_data <- as.matrix(full_data[, covariate_names])
  y_data <- as.matrix(carbon_data)
  
  # Calculate scaling parameters from all data
  scaling_params <- list(
    mean = colMeans(x_data),
    sd = apply(x_data, 2, stats::sd)
  )
  
  # Normalize the data
  x_data_scaled <- scale(x_data, center = scaling_params$mean, scale = scaling_params$sd)
  
  # Split into training and testing sets
  x_train <- x_data_scaled[sample, ]
  x_test <- x_data_scaled[-sample, ]
  y_train <- y_data[sample, ]
  y_test <- y_data[-sample, ]
  
  # Initialize lists for outputs
  models <- list()
  maps <- list()
  predictions <- list()
  
  if (model_type == "rf") {
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
  } else if (model_type == "cnn") {
    # Define k_folds and num_bags
    k_folds <- 5
    num_bags <- 5

    # Reshape input data for CNN
    x_train <- keras3::array_reshape(x_train, c(nrow(x_train), 1, ncol(x_train)))
    x_test <- keras3::array_reshape(x_test, c(nrow(x_test), 1, ncol(x_test)))

    for (i in seq_along(carbon_data)) {
      all_models <- list()
      
      # Create k-fold indices
      fold_indices <- .create_folds(y = seq_len(nrow(x_train)), k = k_folds, list = TRUE, returnTrain = FALSE)

      #initialise operations for each fold
      for (fold in 1:k_folds) {
        cat(sprintf("Processing ensemble %d, fold %d of %d\n", i, fold, k_folds))
        
        # Split data into training and validation sets for this fold
        train_indices <- setdiff(seq_len(nrow(x_train)), fold_indices[[fold]])
        val_indices <- fold_indices[[fold]]
        
        x_train_fold <- x_train[train_indices, , drop = FALSE]
        y_train_fold <- y_train[train_indices, i]
        x_val_fold <- x_train[val_indices, , drop = FALSE]
        y_val_fold <- y_train[val_indices, i]
        
        # Create bagged models for this fold
        fold_models <- list()
        for (bag in 1:num_bags) {
          # Create bootstrap sample
          bootstrap_indices <- sample(1:nrow(x_train_fold), size = nrow(x_train_fold), replace = TRUE)
          x_train_bag <- x_train_fold[bootstrap_indices, ]
          y_train_bag <- y_train_fold[bootstrap_indices]
          
          # Define the CNN model architecture
          # Used dual batch normalization and dropout as the first set of batch normalization and 
          model <- keras3::keras_model_sequential() |>
            # Layer Reshape : Reshape to fit target shape for the convolutional layer
            keras3::layer_reshape(target_shape = c(ncol(x_train), 1, 1), input_shape = ncol(x_train)) |>
            # 1D Convolutional layer: Extracts local features from input data
            keras3::layer_conv_2d(
              filters = 32,
              kernel_size = c(3, 1),
              activation = 'relu',
              padding = 'same'
            ) |>
            # Flatten: Converts 3D output to 1D for dense layer input
            keras3::layer_flatten() |>
            # Dense layer: Learns complex combinations of features
            keras3::layer_dense(
              units = 64, 
              activation = 'relu',
              kernel_regularizer = keras3::regularizer_l2(0.01)
            ) |>
            # Batch normalization: Normalizes layer inputs, stabilizes learning, reduces internal covariate shift
            keras3::layer_batch_normalization() |>
            # Dropout: Randomly sets some of inputs to 0, reducing overfitting and improving generalization
            keras3::layer_dropout(rate = 0.3) |>
            # Dense layer: Learns complex combinations of features
            keras3::layer_dense(
              units = 32, 
              activation = 'relu',
              kernel_regularizer = keras3::regularizer_l2(0.01)
            ) |>
            # Batch normalization: Further stabilizes learning in deeper layers
            keras3::layer_batch_normalization() |>
            # Dropout: Additional regularization to prevent overfitting in final layer
            keras3::layer_dropout(rate = 0.3) |>
            # Output layer: Single neuron for regression prediction
            keras3::layer_dense(
              units = 1,
              kernel_regularizer = keras3::regularizer_l2(0.01)
            )
          
          # Learning rate scheduler
          lr_schedule <- keras3::learning_rate_schedule_exponential_decay(
            initial_learning_rate = 0.001,
            decay_steps = 1000,
            decay_rate = 0.9
          )
          
          # Early stopping callback
          early_stopping <- keras3::callback_early_stopping(
            monitor = 'loss',
            patience = 10,
            restore_best_weights = TRUE
          )

          # Compile the model
          model |> keras3::compile(
            loss = 'mean_squared_error',
            optimizer = keras3::optimizer_adam(learning_rate = lr_schedule),
            metrics = c('mean_absolute_error')
          )

          # Train the model
          model |> keras3::fit(
            x = x_train_bag,
            y = y_train_bag,
            epochs = 500,
            batch_size = 32,
            callbacks = list(early_stopping),
            verbose = 0
          )

          # Store the trained model for this bag in the fold_models list
          fold_models[[bag]] <- model
        }
        
        # Add fold models to all_models list
        all_models <- c(all_models, fold_models)
      }
      
      # Store all models for this ensemble
      models[[i]] <- all_models
      
      # Use all models for predictions
      cnn_ensemble_predict <- function(models, newdata, scaling_params) {
        newdata <- scale(newdata, center = scaling_params$mean, scale = scaling_params$sd)
        predictions <- sapply(models, function(m) stats::predict(m, newdata))
        return(rowMeans(predictions))
      }

      # Create a prediction raster from covariates
      prediction_rast <- terra::rast(covariates)

      # Generate spatial predictions using the trained model
      maps[[i]] <- terra::predict(prediction_rast, model = models[[i]],
                                  fun = cnn_ensemble_predict,
                                  scaling_params = scaling_params)

      # Make predictions on held-out test data
      predictions[[i]] <- cnn_ensemble_predict(models[[i]], x_data[-sample, ], scaling_params)
      
    }
  } else {
    stop("Invalid model_type. Please choose either 'rf' for Random Forest or 'cnn' for Convolutional Neural Network.")
  }
  
  # Organize the results into a single output list
  downscale_output <- list(
    data = list(training = train_data, testing = test_data),
    models = models,
    maps = maps,
    predictions = predictions,
    scaling_params = scaling_params
  )
  
  # Rename each element of the output list with appropriate ensemble numbers
  for (i in seq_along(carbon_data)) {
    names(downscale_output$models)[i] <- paste0("ensemble", i)
    names(downscale_output$maps)[i] <- paste0("ensemble", i)
    names(downscale_output$predictions)[i] <- paste0("ensemble", i)
  }
  
  return(downscale_output)
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
  metrics <- list()
  
  for (i in 1:length(downscale_output$data)) {
    actual <- downscale_output$data[[i]]$testing[[paste0(carbon_pool, "_ens", i)]]
    predicted <- downscale_output$predictions[[i]]
    
    mse <- mean((actual - predicted)^2)
    mae <- mean(abs(actual - predicted))
    r_squared <- 1 - sum((actual - predicted)^2) / sum((actual - mean(actual))^2)
    
    metrics[[i]] <- list(MSE = mse, MAE = mae, R_squared = r_squared, actual = actual, predicted = predicted)
  }
  
  names(metrics) <- paste0("ensemble", seq_along(metrics))
  
  return(metrics)
}
