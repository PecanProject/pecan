##' @title Subset ensemble data for downscaling
##' @name SDA_downscale_preprocess
##' @author Sambhav Dixit, David LeBauer
##'
##' @param ensemble_data EFI standard tibble or data.frame
##' @param site_coords data.frame with unique site id
##' @param date Date. The date for the run, must be a date within `ensemble_data`.
##' @param carbon_pool Character. Carbon pool of interest. Name must match the carbon pool name in ensemble_data.
##' found within the file or object supplied to 'ensemble_data'.
##' @details This function subsets ensemble data and ensures that the specified date and
##' carbon pool are present in the ensemble data.
##'
##' @return A list containing the cleaned site coordinates and the ensemble carbon output for the
##' specified date and carbon pool.
##'
##' @export

subset_ensemble <- function(ensemble_data, site_coords, date, carbon_pool) {
  # Confirm date is in ensemble data
  if (!any(lubridate::date(unique(ensemble_data$datetime)) == lubridate::date(date))) {
    PEcAn.logger::logger.error(paste(
      "Provided date", date,
      "is not found in the ensemble_data input."
    ))
  }

  # Ensure the carbon pool exists in the input data
  if (!carbon_pool %in% unique(ensemble_data$variable)) {
    PEcAn.logger::logger.error("Carbon pool", carbon_pool, "not found in the input data.")
  }

  # Ensure the sites are in the ensemble data
  if (!all(unique(site_coords$site_id) %in% unique(ensemble_data$site_id))) {
    PEcAn.logger::logger.error("Some sites in site_coords are not present in the ensemble_data.")
    # identify which sites are missing
    missing <- setdiff(unique(site_coords$site_id), unique(ensemble_data$site_id))
    setdiff(unique(ensemble_data$site_id), unique(site_coords$site_id))
    length(unique(site_coords$site_id)) # number of sites in site_coords
    length(unique(ensemble_data$site_id)) # number of sites in ensemble_data
  }

  # Filter the ensemble data to the specified date and carbon pool
  ensemble_data <- ensemble_data |>
    dplyr::filter(
      lubridate::date(datetime) == lubridate::date(date),
      site_id %in% unique(site_coords$site_id),
      variable == carbon_pool
    ) |>
    dplyr::select(site_id, ensemble, prediction) # use site_id instead of site

  if (nrow(ensemble_data) == 0) {
    PEcAn.logger::logger.error("No carbon data found for the specified carbon pool.")
  }

  PEcAn.logger::logger.info("Ensemble data subset completed successfully.")
  return(ensemble_data)
}

## Helper function to convert table with lat, lon into an sf object
.convert_coords_to_sf <- function(coords) {
  if (inherits(coords, "sf")) {
    return(coords)
  } else if (is.data.frame(coords)) {
    if (!all(c("lon", "lat") %in% names(coords))) {
      PEcAn.logger::logger.error("Coordinates data frame must contain 'lon' and 'lat'.")
    }
    return(sf::st_as_sf(coords, coords = c("lon", "lat"), crs = 4326))
  } else {
    PEcAn.logger::logger.error("Unsupported coordinates format. Must be an sf object or a data.frame.")
  }
}

## Helper function to convert sf object into table with lat, lon
.convert_sf_to_coords <- function(sf_obj) {
  # Check if it's an sf object
  if (!inherits(sf_obj, "sf")) {
    PEcAn.logger::logger.error("Input must be an 'sf' object.")
  }

  # Extract the geometry into columns named lon/lat
  coord_mat <- sf::st_coordinates(sf_obj)
  colnames(coord_mat) <- c("lon", "lat")

  # Drop the geometry column from the sf, then bind coordinate columns
  out <- sf_obj %>%
    sf::st_drop_geometry() %>%
    tibble::as_tibble() %>%
    dplyr::bind_cols(as.data.frame(coord_mat))
  return(out)
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
    folds <- folds # Test indices are already what we want
  } else {
    folds <- lapply(folds, function(x) indices[-x]) # Return training indices
  }

  if (!list) {
    folds <- unlist(folds)
  }

  return(folds)
}



##' @title Ensemble Downscale
##' @name ensemble_downscale
##' @author Joshua Ploshay, Sambhav Dixit, David LeBauer
##'
##' @param ensemble_data EFI standard tibble or data.frame. Contains carbon data for downscaling.
##' @param site_coords data.frame, tibble, or sf object. Design points. If not sf object, must have
##' 'lon' and 'lat' columns. Must have unique identifier 'site' field.
##' @param covariates table containing numeric predictors to be used in downscaling.
##' Must have unique identifier 'site_id' field and predictor attributes
##' @details This function will downscale forecast data to unmodeled locations using covariates and site locations
##'
##' @return A list containing the model, predictions for all values of covariates as well as test data and test predictions for downstream
##' statistics.
##'
##' @export

ensemble_downscale <- function(ensemble_data, site_coords, covariates) {
  ## TODO
  ## - Accept raster stack as covariates
  ## - Split into separate train and predict functions
  ## - Add CNN functionality, use tidymodels?

  # Dynamically get covariate names
  covariate_names <- colnames(covariates |> dplyr::select(-site_id))
  # Drop zero-variance predictors (lead to NaN on scale())
  if (length(covariate_names) > 0) {
    cov_num <- as.data.frame(covariates[, covariate_names, drop = FALSE])
    var0 <- vapply(cov_num, function(x) stats::var(x, na.rm = TRUE), numeric(1))
    drop_cols <- names(var0)[is.finite(var0) & var0 == 0]
    if (length(drop_cols) > 0) {
      PEcAn.logger::logger.warn(
        "Dropping zero-variance predictors:", paste(drop_cols, collapse = ", ")
      )
      covariate_names <- setdiff(covariate_names, drop_cols)
    }
  }

  # scale to N(0,1) (defaults of scale function)
  scaled_covariates <- covariates |>
    dplyr::mutate(dplyr::across(dplyr::all_of(covariate_names), scale))
  # Assert no NA/NaN/Inf after scaling (data should be cleaned earlier)
  if (length(covariate_names) > 0) {
    preds_df <- as.data.frame(scaled_covariates[, covariate_names, drop = FALSE])
    bad_cols <- names(which(colSums(!is.finite(as.matrix(preds_df))) > 0 | colSums(is.na(preds_df)) > 0))
    if (length(bad_cols) > 0) {
      PEcAn.logger::logger.severe(
        "Non-finite values present after scaling in predictors:",
        paste(bad_cols, collapse = ", "),
        " <U+2014> please fix upstream covariates."
      )
    }
  }

  # Create a single data frame with all predictors and ensemble data
  design_pt_data <- ensemble_data |> # from SIPNET ensemble runs
    dplyr::left_join(scaled_covariates, by = "site_id") # n = nrow(site_coords) * ensemble_size

  # Split the observations into training and testing sets
  n_sites <- nrow(design_pt_data)

  ## TODO: Use groups from the 01_cluster_and_select_design_points.R
  global_idx <- if (n_sites > 0) sample(seq_len(n_sites), size = max(1, round(0.8 * n_sites))) else integer(0)
  train_data <- if (length(global_idx) > 0) design_pt_data[global_idx, , drop = FALSE] else design_pt_data[0, , drop = FALSE]
  test_data <- if (length(global_idx) > 0) design_pt_data[-global_idx, , drop = FALSE] else design_pt_data[0, , drop = FALSE]

  ensembles <- unique(ensemble_data$ensemble)
  n_ensembles <- length(ensembles)

  PEcAn.logger::logger.info(
    paste("Start downscaling with", n_ensembles, "ensembles.")
  )

  results <- furrr::future_map(seq_along(ensembles), function(i) {
    ens_label <- ensembles[i]
    formula <- as.formula(
      paste("prediction ~", paste(covariate_names, collapse = " + "))
    )

    # Build a per-ensemble stratified split (80/20), guaranteeing at least 1 train row
    ens_data <- design_pt_data |>
      dplyr::filter(ensemble == ens_label)
    # Assert no NA in per-ensemble predictors (from join) before splitting
    if (length(covariate_names) > 0) {
      ens_preds <- as.data.frame(ens_data[, covariate_names, drop = FALSE])
      if (nrow(ens_preds) > 0) {
        na_cols_ens <- names(which(colSums(is.na(ens_preds)) > 0))
        if (length(na_cols_ens) > 0) {
          # Identify a small sample of problematic site_ids for debugging
          bad_rows <- which(!stats::complete.cases(ens_preds))
          bad_sites <- unique(ens_data$site_id[bad_rows])
          PEcAn.logger::logger.error(
            "NA found in per-ensemble predictors after join for ensemble ", ens_label,
            "; columns: ", paste(na_cols_ens, collapse = ", "),
            "; example site_ids: ", paste(utils::head(bad_sites, 10), collapse = ", ")
          )
        }
      }
    }
    n <- nrow(ens_data)
    if (n <= 9) {
      PEcAn.logger::logger.severe(glue::glue("Only {n} records available for ensemble {ens_label} at selected slice"))
    }
    if (n >= 10) {
      n_train <- max(1, floor(0.8 * n))
      idx <- sample(seq_len(n), size = n_train)
      .train_data <- ens_data[idx, , drop = FALSE]
      .test_data <- ens_data[-idx, , drop = FALSE]
    }

    PEcAn.logger::logger.info(
      glue::glue("Fitting model for ensemble {ens_label} ({i}/{n_ensembles}) with {nrow(.train_data)} training points and {nrow(.test_data)} testing points.")
    )

    # nodesize is just a temporary hack
    nodesize <- ifelse(nrow(.train_data) < 50, 1, 5)
    PEcAn.logger::logger.info("Using nodesize=", nodesize, " for ensemble ", ens_label)
    # Assert no NA in training predictors or response (do not impute/drop)
    if (length(covariate_names) > 0) {
      train_preds <- as.data.frame(.train_data[, covariate_names, drop = FALSE])
      na_cols <- names(which(colSums(is.na(train_preds)) > 0))
      if (length(na_cols) > 0) {
        PEcAn.logger::logger.error(
          "NA found in training predictors for ensemble ", ens_label,
          ": ", paste(na_cols, collapse = ", "),
          ". Please fix upstream covariates for these site_ids."
        )
      }
    }
    if (any(is.na(.train_data$prediction))) {
      PEcAn.logger::logger.error(
        "NA found in training response 'prediction' for ensemble ", ens_label,
        ". Please fix upstream ensemble data."
      )
    }
    # Ensure training proceeds even if global na.action is set to na.fail
    model <- randomForest::randomForest(formula,
      data = .train_data,
      ntree = 1000,
      keep.forest = TRUE,
      importance = TRUE,
      nodesize = nodesize,
      na.action = stats::na.fail
    )

    # TODO: enable alternative for speed once RF gets slow:
    # model <- ranger::ranger(
    #   formula,
    #   data = .train_data,
    #   num.trees = 1000, # correct replacement for ntree
    #   importance = "impurity", # matches randomForest default
    #   num.threads = 4 # optional, speeds up predict()
    # )
    PEcAn.logger::logger.info(
      "Predicting for ensemble", paste0(" ", ens_label, " (", i, "/", n_ensembles, ")"),
      "with", nrow(scaled_covariates), "design points."
    )
    start <- Sys.time()
    prediction <- predict(model, scaled_covariates)
    end <- Sys.time()
    ### Optimization notes for when we scale up:
    ### for speed as this scales up use ranger::predict
    # prediction <- ranger::predict(model, scaled_covariates,
    #   num.threads = parallel::detectCores() - 1)
    ### If predict runs out of memory, can split / apply predict / and combine results
    # split_rows <- split(scaled_covariates, ceiling(seq_len(nrow(scaled_covariates)) / 10000))
    # preds <- purrr::map(split_rows, ~ ranger::predict(model, .x))
    # prediction <- do.call(rbind, lapply(preds, function(x) x$predictions))

    ### for raster stack as covariates
    # prediction <- terra::predict(model, scaled_covariates)

    PEcAn.logger::logger.info(
      "Prediction for ensemble", i, "completed in",
      round(as.numeric(end - start, units = "secs"), 2), "seconds."
    )


    # Predicting for test data should be much faster
    if (length(covariate_names) > 0 && nrow(.test_data) > 0) {
      test_preds <- as.data.frame(.test_data[, covariate_names, drop = FALSE])
      na_cols_test <- names(which(colSums(is.na(test_preds)) > 0))
      if (length(na_cols_test) > 0) {
        bad_rows_test <- which(!stats::complete.cases(test_preds))
        bad_sites_test <- unique(.test_data$site_id[bad_rows_test])
        PEcAn.logger::logger.error(
          "NA found in test predictors for ensemble ", ens_label,
          "; columns: ", paste(na_cols_test, collapse = ", "),
          "; example site_ids: ", paste(utils::head(bad_sites_test, 10), collapse = ", ")
        )
      }
    }
    test_prediction <- predict(model, .test_data)

    list(
      model = model,
      prediction = prediction,
      test_data = .test_data,
      test_prediction = test_prediction
    )
  },
  .progress = TRUE,
  .options = furrr::furrr_options(seed = TRUE) # Use global seed to silence warnings
  )

  # Organize the results into a single output list
  # TODO: need to disambiguate terms design point, sipnet prediction @ design points (which become 'test'
  # vs downscaling prediction
  downscale_output <-
    list(
      data = list(training = train_data, testing = test_data), # should these be the scaled versions?
      model = purrr::map(results, "model"),
      predictions = purrr::map(results, "prediction"),
      test_data = purrr::map(results, "test_data"),
      test_predictions = purrr::map(results, "test_prediction")
    )

  return(downscale_output)
}

##' @title Calculate Metrics for Downscaling Results
##' @name downscale_metrics
##' @author Sambhav Dixit, David LeBauer
##'
##' @param downscale_output List. Output from the downscale function, containing data, models, maps, predictions,
##' and test predictions for each ensemble.
##' @param carbon_pool Character. Name of the carbon pool used in the downscaling process.
##'
##' @details This function calculates performance metrics for the downscaling results. It computes Mean Squared Error (MSE),
##' Mean Absolute Error (MAE), and R-squared for each ensemble. The function uses the actual values from the testing data and
##' the predictions generated during the downscaling process.
##'
##' @description This function takes the output from the downscale function and computes various performance metrics for each ensemble.
##' It provides a way to evaluate the accuracy of the downscaling results without modifying the main downscaling function.
##'
##' @return A list of metrics for each ensemble, where each element contains MAE , RMSE ,R_squared ,CV,
##' and actual values from testing data and predicted values for the testing data
##'
##' @export
downscale_metrics <- function(downscale_output) {
  test_data_list <- lapply(downscale_output$test_data, function(x) dplyr::pull(x, prediction))
  predicted_list <- downscale_output$test_predictions

  metric_fn <- function(actual, predicted) { # Could use PEcAn.benchmark pkg?

    # Ensure no NA values in actual and predicted
    if (any(is.na(actual)) || any(is.na(predicted))) {
      n_na_act <- sum(is.na(actual))
      n_na_pred <- sum(is.na(predicted))
      PEcAn.logger::logger.error(
        "NA values found in actual or predicted data (",
        n_na_act, " actual; ", n_na_pred, " predicted )."
      )
    }
    mean <- mean(actual)
    RMSE <- sqrt(mean((actual - predicted)^2))
    MAE <- mean(abs(actual - predicted))

    ss_res <- sum((actual - predicted)^2)
    ss_tot <- sum((actual - mean(actual))^2)
    R2 <- if (ss_tot == 0) NA_real_ else 1 - ss_res / ss_tot

    CV <- if (mean == 0) NA_real_ else 100 * RMSE / mean

    if (!is.na(CV) && CV > 500) {
      PEcAn.logger::logger.warn(
        "CV > 500 (", round(CV, 1), "), indicating CV may not be an appropriate metric"
      )
      CV <- NA
    }

    stats <- data.frame(
      mean = mean,
      RMSE = RMSE,
      MAE = MAE,
      R2 = R2,
      CV = CV
    ) |>
      signif(digits = 2)
  }
  metrics <- purrr::map2(test_data_list, predicted_list, metric_fn) |>
    dplyr::bind_rows(.id = "ensemble")

  return(metrics)
}
