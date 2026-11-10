##' @title Preprocess Data for Downscaling
##' @name SDA_downscale_preprocess
##' @author Sambhav Dixit
##' @author David LeBauer
##'
##' @param ensemble_data List where each element is a data.frame indexed by date.
##' @param site_coords A data.frame with `lon` and `lat` columns, or an sf object with point geometries.
##' @param date Date. The date for the run, which must occur within `ensemble_data`.
##' @param carbon_pool Character. Carbon pool of interest. Name must match a column in `ensemble_data`.
##' @details This function ensures that the specified date and carbon pool are present in the input data. It also checks the validity of the site coordinates and requires site coordinates and carbon data to have the same number of rows.
##'
##' @description This function checks the input data, ensuring that the required date and carbon pool exist, and that the site coordinates are valid.
##'
##' @return A list containing the input data, cleaned site coordinates, and preprocessed carbon data.

SDA_downscale_preprocess <- function(ensemble_data, site_coords, date, carbon_pool) {
  input_data <- ensemble_data
  site_coordinates <- site_coords

  # If sf object, convert to data.frame with 'lon' and 'lat'
  if (inherits(site_coordinates, "sf")) {
    site_coordinates <- sf::st_coordinates(site_coordinates) |>
      dplyr::rename(lon = .data$X, lat = .data$Y) |>
      dplyr::bind_cols(
        site_coordinates
      )
  }

  # Ensure site coordinates have 'lon' and 'lat' columns
  if (!all(c("lon", "lat") %in% names(site_coordinates))) {
    PEcAn.logger::logger.error("Site coordinates must contain 'lon' and 'lat' columns.")
  }

  # Standardize input_data date names YYYY-MM-DD
  input_date_names <- lubridate::ymd(names(input_data))
  names(input_data) <- input_date_names

  standard_date <- lubridate::ymd(date)

  # Ensure the date exists in the input data
  if (!standard_date %in% input_date_names) {
    PEcAn.logger::logger.error(paste("Date", standard_date, "not found in the input data."))
  }

  # Extract the carbon data for the specified focus year
  index <- which(input_date_names == standard_date)
  data <- input_data[[index]]

  # Ensure the carbon pool exists in the input data
  if (!carbon_pool %in% names(data)) {
    PEcAn.logger::logger.error("Carbon pool", carbon_pool, "not found in the input data.")
  }

  carbon_data <- as.data.frame(t(data[which(names(data) == carbon_pool)]))
  names(carbon_data) <- paste0("ensemble", seq(ncol(carbon_data)))

  # Ensure the number of rows in site coordinates matches the number of rows in carbon data
  if (nrow(site_coordinates) != nrow(carbon_data)) {
    PEcAn.logger::logger.info(
      "Number of rows in site coordinates does not match the number of rows in carbon data."
    )
    PEcAn.logger::logger.info(
      "There are", nrow(site_coordinates), "row(s) in site coordinates and",
      nrow(carbon_data), "row(s) in carbon data."
    )
    PEcAn.logger::logger.severe("I am not sure how to reconcile these differences.")
  }

  PEcAn.logger::logger.info("Preprocessing completed successfully.")
  return(list(input_data = input_data, site_coordinates = site_coordinates, carbon_data = carbon_data))
}

##' @title SDA Downscale Function
##' @name SDA_downscale
##' @author Joshua Ploshay, Sambhav Dixit
##'
##' @param preprocessed List. Preprocessed data returned as an output from the SDA_downscale_preprocess function.
##' @param carbon_pool Character. Carbon pool of interest. Name must match the carbon pool in `preprocessed`.
##' @param covariates SpatRaster stack or sf object. Used as predictors in downscaling. If providing a raster stack, layers should be named. If providing an sf object, predictor attributes should be present.
##' @param model_type Character. Either "rf" for Random Forest or "cnn" for Convolutional Neural Network. Default is Random Forest.
##' @param seed Numeric or NULL. Optional seed for random number generation. Default is NULL.
##' @details This function will downscale forecast data to unmodeled locations using covariates and site locations
##'
##' @description This function uses either Random Forest or Convolutional Neural Network model based on the model_type parameter.
##'
##' @return A list containing the training and testing data sets, models, predicted maps for each ensemble member, and predictions for testing data.

SDA_downscale <- function(preprocessed, carbon_pool, covariates, model_type = c("rf", "cnn"), seed = NULL) {
  model_type <- match.arg(model_type)
  carbon_data <- preprocessed$carbon_data

  # Convert site coordinates to an sf object using the helper function
  site_coordinates_sf <- .convert_coords_to_sf(preprocessed$site_coordinates)

  # Extract predictors from SpatRaster or sf covariates
  if (inherits(covariates, "SpatRaster")) {
    # Extract raster values at the site coordinates
    predictors <- as.data.frame(terra::extract(covariates, terra::vect(site_coordinates_sf), ID = FALSE))
  } else if (inherits(covariates, "sf")) {
    # For sf objects, spatial join
    predictors <- sf::st_drop_geometry(sf::st_join(site_coordinates_sf, covariates, join = sf::st_intersects))
  } else {
    PEcAn.logger::logger.error("Unsupported covariates object. Must be a SpatRaster or an sf object.")
  }

  # Dynamically get covariate names
  covariate_names <- names(predictors)

  # Create a single data frame with all predictors and ensemble data
  full_data <- cbind(carbon_data, predictors)

  # Split the observations into training and testing sets
  if (!is.null(seed)) {
    set.seed(seed) # Only set seed if provided
  }
  sample <- sample(seq_len(nrow(full_data)),
    size = round(0.75 * nrow(full_data))
  )
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
# TODO: Consider extracting the RF and CNN branches into dedicated helper functions.
  if (model_type == "rf") {
    for (i in seq_along(carbon_data)) {
      ensemble_col <- paste0("ensemble", i)
      formula <- stats::as.formula(paste(ensemble_col, "~", paste(covariate_names, collapse = " + ")))
      models[[i]] <- randomForest::randomForest(
        formula,
        data = train_data,
        ntree = 1000,
        na.action = stats::na.omit,
        keep.forest = TRUE,
        importance = TRUE
      )
# TODO: Separate model fitting and held-out evaluation from spatial map prediction,
# so models can be evaluated without the cost of generating raster maps.
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

      # initialise operations for each fold
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
          bootstrap_indices <- sample(seq_len(x_train_fold), size = nrow(x_train_fold), replace = TRUE)
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
              activation = "relu",
              padding = "same"
            ) |>
            # Flatten: Converts 3D output to 1D for dense layer input
            keras3::layer_flatten() |>
            # Dense layer: Learns complex combinations of features
            keras3::layer_dense(
              units = 64,
              activation = "relu",
              kernel_regularizer = keras3::regularizer_l2(0.01)
            ) |>
            # Batch normalization: Normalizes layer inputs, stabilizes learning, reduces internal covariate shift
            keras3::layer_batch_normalization() |>
            # Dropout: Randomly sets some of inputs to 0, reducing overfitting and improving generalization
            keras3::layer_dropout(rate = 0.3) |>
            # Dense layer: Learns complex combinations of features
            keras3::layer_dense(
              units = 32,
              activation = "relu",
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
            monitor = "loss",
            patience = 10,
            restore_best_weights = TRUE
          )

          # Compile the model
          model |> keras3::compile(
            loss = "mean_squared_error",
            optimizer = keras3::optimizer_adam(learning_rate = lr_schedule),
            metrics = c("mean_absolute_error")
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
      maps[[i]] <- terra::predict(prediction_rast,
        model = models[[i]],
        fun = cnn_ensemble_predict,
        scaling_params = scaling_params
      )

      # Make predictions on held-out test data
      predictions[[i]] <- cnn_ensemble_predict(models[[i]], x_data[-sample, ], scaling_params)
    }
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

  for (i in seq_along(downscale_output$data)) {
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
