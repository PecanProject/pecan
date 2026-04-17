## Load packages
library(data.table)
library(dplyr)
library(xgboost)
library(parallel)
library(doParallel)
library(caret)

##' @title parse_bcm_args
##' @name parse_bcm_args
##' @author Yang Gu
##'
##' @param args Character vector from commandArgs(trailingOnly = TRUE).
##'
##' @description
##' Parse command-line arguments for flux type and response variable.
##' The first argument is the flux type ("LE" or "NEE"), and the second
##' argument is the raw ensemble variable name (ens01).
##'
##' @return
##' A list containing `flux_type`, `raw_response_var`, and `response_var`.
parse_bcm_args <- function(args) {
  flux_type <- if (length(args) >= 1) toupper(args[1]) else "LE"
  raw_response_var <- if (length(args) >= 2) args[2] else "ens01"
  
  if (!flux_type %in% c("LE", "NEE")) {
    stop("flux_type must be either 'LE' or 'NEE'.")
  }
  
  response_var <- paste0(raw_response_var, "_residual")
  
  return(list(
    flux_type = flux_type,
    raw_response_var = raw_response_var,
    response_var = response_var
  ))
}

##' @title get_flux_config
##' @name get_flux_config
##' @author Yang Gu
##'
##' @param flux_type Character string, either "LE" or "NEE".
##'
##' @description
##' Return flux-specific configuration, including observation column names,
##' model mean column names, columns to remove, and output suffixes.
##'
##' @return
##' A list of flux-specific settings.
get_flux_config <- function(flux_type) {
  if (flux_type == "LE") {
    return(list(
      flux_type = "LE",
      mean_col = "Qle_mean",
      mean_std_col = "flux_mean",
      obs_col = "LE_F_MDS",
      obs_qc_col = "LE_F_MDS_QC",
      residual_col = "LE_residual",
      corrected_col = "LE_corrected",
      pred_residual_col = "LE_resi_pred",
      gfed_file = "/projectnb/dietzelab/guYANG/Gap_fill/results/le_ens_ec_3h.csv",
      output_tag = "le"
    ))
  }
  
  if (flux_type == "NEE") {
    return(list(
      flux_type = "NEE",
      mean_col = "NEE_mean",
      mean_std_col = "flux_mean",
      obs_col = "NEE_CUT_USTAR50",
      obs_qc_col = "NEE_CUT_USTAR50_QC",
      residual_col = "NEE_residual",
      corrected_col = "NEE_corrected",
      pred_residual_col = "NEE_resi_pred",
      gfed_file = "/projectnb/dietzelab/guYANG/Gap_fill/results/ens_ec_3h.csv",
      output_tag = "nee"
    ))
  }
  
  stop("Unsupported flux_type.")
}

##' @title read_close_points_and_envres
##' @name read_close_points_and_envres
##' @author Yang Gu
##'
##' @param matched_file Path to matched point-site file.
##' @param envres_file Path to .RData file containing `envres`.
##'
##' @description
##' Read the nearest matched site-index table and load the integrated envres
##' dataset from .RData.
##'
##' @return
##' A list containing `close_points_df` and `envres`.
read_close_points_and_envres <- function(matched_file, envres_file) {
  close_points_df <- fread(matched_file)
  setDT(close_points_df)
  close_points_df <- close_points_df[order(min_dist_m), .SD[1], by = index]
  
  load(envres_file)
  
  return(list(
    close_points_df = close_points_df,
    envres = envres
  ))
}

##' @title filter_envres_for_flux
##' @name filter_envres_for_flux
##' @author Yang Gu
##'
##' @param envres Environmental result table.
##' @param flux_cfg Flux-specific configuration list.
##'
##' @description
##' Remove flux-specific observed columns, filter land-cover classes,
##' and restrict the date range used in BCM.
##'
##' @return
##' A filtered data.table.
filter_envres_for_flux <- function(envres, flux_cfg) {
  cols_to_drop <- intersect(
    c(flux_cfg$obs_col, flux_cfg$obs_qc_col, flux_cfg$residual_col),
    names(envres)
  )
  if (length(cols_to_drop) > 0) {
    envres[, (cols_to_drop) := NULL]
  }
  
  envres <- envres[as.numeric(as.character(LC)) <= 4]
  
  cutoff_date <- as.POSIXct("2012-07-15 23:59:59", tz = "UTC")
  envres <- envres[utc > cutoff_date]
  
  return(envres)
}

##' @title add_gapfill_and_predictors
##' @name add_gapfill_and_predictors
##' @author Yang Gu
##'
##' @param envres Environmental result table.
##' @param gfed Gap-fill ensemble result table.
##' @param close_points_df Nearest site-index table.
##' @param raw_response_var Raw ensemble variable name, e.g. "ens01".
##' @param flux_cfg Flux-specific configuration list.
##'
##' @description
##' Merge a selected ensemble gap-fill column into `envres`, add predictor
##' transformations, standardize the model mean column to `flux_mean`,
##' and construct the response residual.
##'
##' @return
##' A processed data.table ready for BCM modeling.
add_gapfill_and_predictors <- function(envres,
                                       gfed,
                                       close_points_df,
                                       raw_response_var,
                                       flux_cfg) {
  cols_to_merge <- c(raw_response_var)
  gfed_sub <- gfed[, c("Site_ID", "utc", cols_to_merge), with = FALSE]
  
  setkey(envres, Site_ID, utc)
  setkey(gfed_sub, Site_ID, utc)
  envres <- gfed_sub[envres]
  
  envres[, c("LC", "is_day", "KGC") := lapply(.SD, as.factor),
         .SDcols = c("LC", "is_day", "KGC")]
  
  envres[, t2m2 := t2m^2]
  envres[, PPFD2 := PPFD^2]
  envres[, EVI2 := EVI^2]
  envres[, VPD_t2m := VPD * t2m]
  envres[, sincos := sin_doy * cos_doy]
  
  envres[, Site_ID := NULL]
  
  setkey(envres, index)
  setkey(close_points_df, index)
  envres[close_points_df, Site_ID := i.Site_ID]
  
  envres[EVI == -3000, EVI := NA_real_]
  envres[NDVI == -3000, NDVI := NA_real_]
  
  if (flux_cfg$mean_col %in% names(envres) &&
      flux_cfg$mean_col != flux_cfg$mean_std_col) {
    setnames(envres, old = flux_cfg$mean_col, new = flux_cfg$mean_std_col)
  }
  
  cols_ens <- grep("ens", names(envres), value = TRUE)
  cols_to_keep <- raw_response_var
  cols_to_remove <- setdiff(cols_ens, cols_to_keep)
  if (length(cols_to_remove) > 0) {
    envres[, (cols_to_remove) := NULL]
  }
  
  response_residual_col <- paste0(raw_response_var, "_residual")
  envres[, (response_residual_col) := get(flux_cfg$mean_std_col) - get(raw_response_var)]
  
  return(envres)
}

##' @title split_bcm_data
##' @name split_bcm_data
##' @author Yang Gu
##'
##' @param envres Environmental result table.
##' @param response_var Response column name.
##'
##' @description
##' Split the data into observed-site training data and unobserved-site
##' prediction data.
##'
##' @return
##' A list containing `tenvres` and `pdat`.
split_bcm_data <- function(envres, response_var) {
  indices_to_keep <- envres[!is.na(Site_ID), unique(index)]
  
  tenvres <- envres[index %in% indices_to_keep]
  setDT(tenvres)
  tenvres[, LC := factor(as.character(LC))]
  tenvres <- tenvres[!is.na(get(response_var))]
  tenvres[, `:=`(
    LC     = as.factor(LC),
    is_day = as.factor(is_day),
    KGC    = as.factor(KGC),
    index  = as.character(index)
  )]
  
  envres <- envres[!(index %in% indices_to_keep)]
  pdat <- envres[is.na(get(response_var))]
  
  return(list(
    tenvres = tenvres,
    pdat = pdat
  ))
}

##' @title get_bcm_model_components
##' @name get_bcm_model_components
##' @author Yang Gu
##'
##' @description
##' Return the predictor list, formula, and XGBoost hyperparameter settings
##' used in the BCM workflow.
##'
##' @return
##' A list of modeling components.
get_bcm_model_components <- function() {
  predictors_all <- c(
    "t2m", "sp", "d2m", "tp", "strd", "KGC", "lon",
    "sin_doy", "cos_doy", "NDVI", "EVI", "twi", "PH", "Sand", "lat",
    "agb", "SOC", "N", "year_since_disturb", "PPFD", "WindSpeed",
    "t2m2", "PPFD2", "EVI2", "VPD_t2m", "sincos",
    "sm_0_10", "sm_10_40", "sm_40_100", "sm_100_200",
    "VPD", "is_day"
  )
  
  pred_formula <- as.formula(
    paste0("~ ", paste(predictors_all, collapse = " + "), " - 1")
  )
  
  ### TBD: set the specific settings for different LC type
  lc_param_list <- list(
    `10` = list(objective = "reg:squarederror", eta = 0.02,  max_depth = 4,
                subsample = 0.5,  colsample_bytree = 0.5,
                min_child_weight = 20, gamma = 2, lambda = 4, alpha = 3),
    `40` = list(objective = "reg:squarederror", eta = 0.025, max_depth = 5,
                subsample = 0.55, colsample_bytree = 0.6,
                min_child_weight = 15, gamma = 1, lambda = 4, alpha = 2),
    `50` = list(objective = "reg:squarederror", eta = 0.015, max_depth = 3,
                subsample = 0.35, colsample_bytree = 0.4,
                min_child_weight = 40, gamma = 4, lambda = 6, alpha = 5),
    `60` = list(objective = "reg:squarederror", eta = 0.02,  max_depth = 3,
                subsample = 0.4,  colsample_bytree = 0.5,
                min_child_weight = 25, gamma = 3, lambda = 4, alpha = 3),
    `70` = list(objective = "reg:squarederror", eta = 0.02,  max_depth = 3,
                subsample = 0.4,  colsample_bytree = 0.4,
                min_child_weight = 30, gamma = 4, lambda = 5, alpha = 4)
  )
  
  default_params <- list(
    objective = "reg:squarederror", eta = 0.1, max_depth = 6,
    subsample = 0.8, colsample_bytree = 0.8,
    min_child_weight = 10, gamma = 0.5, lambda = 3, alpha = 2
  )
  
  return(list(
    predictors_all = predictors_all,
    pred_formula = pred_formula,
    lc_param_list = lc_param_list,
    default_params = default_params,
    nrounds_fixed = 200
  ))
}

##' @title get_valid_lc_tasks
##' @name get_valid_lc_tasks
##' @author Yang Gu
##'
##' @param tenvres Training data.table.
##'
##' @description
##' Construct leave-one-site-out CV tasks for LC classes with more than one site.
##'
##' @return
##' A data.frame of CV tasks.
get_valid_lc_tasks <- function(tenvres) {
  lc_counts <- tenvres %>%
    distinct(LC, index) %>%
    group_by(LC) %>%
    summarise(n_sites = n(), .groups = "drop")
  
  valid_lcs <- lc_counts %>%
    filter(n_sites > 1) %>%
    pull(LC)
  
  tasks <- tenvres %>%
    filter(LC %in% valid_lcs) %>%
    distinct(LC, index) %>%
    arrange(LC, index)
  
  return(tasks)
}

##' @title run_bcm_cv
##' @name run_bcm_cv
##' @author Yang Gu
##'
##' @param tenvres Training data.table.
##' @param tasks CV task table.
##' @param predictors_all Predictor column names.
##' @param response_var Response column name.
##' @param nrounds_fixed Fixed number of boosting rounds.
##' @param lc_param_list LC-specific XGBoost parameter list.
##' @param default_params Default XGBoost parameter list.
##' @param flux_cfg Flux-specific configuration list.
##'
##' @description
##' Run leave-one-site-out cross validation within land-cover classes.
##'
##' @return
##' A data.frame of CV predictions and diagnostics.
run_bcm_cv <- function(tenvres,
                       tasks,
                       predictors_all,
                       response_var,
                       nrounds_fixed,
                       lc_param_list,
                       default_params,
                       flux_cfg) {
  ncores <- max(detectCores() - 1, 1)
  cl <- makeCluster(ncores)
  registerDoParallel(cl)
  
  cv_results <- foreach(
    i = seq_len(nrow(tasks)),
    .packages = c("dplyr", "xgboost", "data.table"),
    .combine  = rbind,
    .export   = c("tenvres", "predictors_all", "response_var",
                  "nrounds_fixed", "tasks", "lc_param_list",
                  "default_params", "flux_cfg")
  ) %dopar% {
    lc <- tasks$LC[i]
    site <- tasks$index[i]
    
    df_lc <- tenvres %>% filter(LC == lc)
    train_df <- df_lc %>% filter(index != site)
    test_df  <- df_lc %>% filter(index == site)
    
    if (nrow(train_df) == 0 || nrow(test_df) == 0) {
      return(NULL)
    }
    
    train_x <- data.matrix(train_df[, predictors_all, with = FALSE])
    test_x  <- data.matrix(test_df[, predictors_all, with = FALSE])
    
    train_y <- train_df[[response_var]]
    test_y  <- test_df[[response_var]]
    
    dtrain <- xgb.DMatrix(data = train_x, label = train_y, missing = NA)
    dtest  <- xgb.DMatrix(data = test_x, label = test_y, missing = NA)
    watchlist <- list(train = dtrain, eval = dtest)
    
    params <- if (as.character(lc) %in% names(lc_param_list)) {
      lc_param_list[[as.character(lc)]]
    } else {
      default_params
    }
    
    model_i <- xgb.train(
      params = params,
      data = dtrain,
      nrounds = nrounds_fixed,
      watchlist = watchlist,
      eval_metric = "rmse",
      early_stopping_rounds = 20,
      nthread = 1,
      verbose = 0
    )
    
    preds_i <- predict(model_i, test_x)
    ev <- as.data.frame(model_i$evaluation_log)
    
    final_train_rmse <- ev$train_rmse[nrow(ev)]
    final_eval_rmse  <- ev$eval_rmse[nrow(ev)]
    best_iter        <- which.min(ev$eval_rmse)
    
    n_check <- 5
    if (nrow(ev) >= n_check + 1) {
      train_tail <- tail(ev$train_rmse, n_check + 1)
      eval_tail  <- tail(ev$eval_rmse, n_check + 1)
      train_trend <- diff(train_tail)
      eval_trend  <- diff(eval_tail)
      has_overfit <- all(train_trend < 0) && all(eval_trend > 0)
    } else {
      has_overfit <- FALSE
    }
    
    rmse_gap   <- final_eval_rmse - final_train_rmse
    rmse_ratio <- final_eval_rmse / final_train_rmse
    
    n <- nrow(test_df)
    
    data.frame(
      LC         = rep(lc, n),
      index      = rep(site, n),
      utc        = test_df$utc,
      flux_mean  = test_df[[flux_cfg$mean_std_col]],
      observed   = test_df[[response_var]],
      predicted  = preds_i,
      train_rmse = rep(final_train_rmse, n),
      eval_rmse  = rep(final_eval_rmse, n),
      rmse_gap   = rep(rmse_gap, n),
      rmse_ratio = rep(rmse_ratio, n),
      best_iter  = rep(best_iter, n),
      has_overfit = rep(has_overfit, n),
      stringsAsFactors = FALSE
    )
  }
  
  stopCluster(cl)
  registerDoSEQ()
  
  cv_results <- cv_results %>%
    mutate(
      flux_pred = flux_mean - predicted,
      flux_obs  = flux_mean - observed
    )
  
  return(cv_results)
}

##' @title predict_bcm
##' @name predict_bcm
##' @author Yang Gu
##'
##' @param tenvres Training data.table.
##' @param pdat Prediction data.table.
##' @param response_var Response column name.
##' @param pred_formula Formula used for model.matrix construction.
##' @param predictors_all Predictor column names.
##' @param nrounds_fixed Fixed number of boosting rounds.
##' @param lc_param_list LC-specific XGBoost parameter list.
##' @param default_params Default XGBoost parameter list.
##' @param flux_cfg Flux-specific configuration list.
##'
##' @description
##' Fit one model per LC using all observed sites and predict residual
##' corrections for unobserved points.
##'
##' @return
##' A prediction data.table with flux-corrected outputs.
predict_bcm <- function(tenvres,
                                   pdat,
                                   response_var,
                                   pred_formula,
                                   predictors_all,
                                   nrounds_fixed,
                                   lc_param_list,
                                   default_params,
                                   flux_cfg) {
  pdat[, flux_resi_pred := NA_real_]
  
  for (lc in levels(tenvres$LC)) {
    df_lc <- tenvres[LC == lc]
    if (nrow(df_lc) < 10) next
    
    mf_train_lc <- model.frame(pred_formula, data = df_lc, na.action = na.pass)
    mat_train_lc <- model.matrix(pred_formula, data = mf_train_lc)
    label_train_lc <- df_lc[[response_var]]
    dtrain_lc <- xgb.DMatrix(data = mat_train_lc, label = label_train_lc, missing = NA)
    
    params_lc <- if (lc %in% names(lc_param_list)) lc_param_list[[lc]] else default_params
    
    bst_lc <- xgb.train(
      params = params_lc,
      data = dtrain_lc,
      nrounds = nrounds_fixed,
      verbose = 0
    )
    
    pdat_lc <- pdat[LC == lc]
    if (nrow(pdat_lc) == 0) next
    
    mf_p_lc <- model.frame(pred_formula, data = pdat_lc, na.action = na.pass)
    mat_p_lc <- model.matrix(pred_formula, data = mf_p_lc)
    dtest_lc <- xgb.DMatrix(data = mat_p_lc, missing = NA)
    preds_lc <- predict(bst_lc, dtest_lc)
    
    pdat[LC == lc, flux_resi_pred := preds_lc]
  }
  
  pdat[, flux_corrected := get(flux_cfg$mean_std_col) - flux_resi_pred]
  
  if (flux_cfg$pred_residual_col != "flux_resi_pred") {
    pdat[, (flux_cfg$pred_residual_col) := flux_resi_pred]
  }
  if (flux_cfg$corrected_col != "flux_corrected") {
    pdat[, (flux_cfg$corrected_col) := flux_corrected]
  }
  
  return(pdat)
}

##' @title compute_bcm_metrics
##' @name compute_bcm_metrics
##' @author Yang Gu
##'
##' @param cv_results Cross-validation result table.
##'
##' @description
##' Compute site-level, LC-level, and corrected-vs-baseline R2 summaries,
##' as well as overfitting diagnostics.
##'
##' @return
##' A list of summary tables.
compute_bcm_metrics <- function(cv_results) {
  r2_by_site <- cv_results %>%
    group_by(LC, index) %>%
    summarise(
      R2 = 1 - sum((observed - predicted)^2) /
        sum((observed - mean(observed))^2),
      .groups = "drop"
    )
  
  r2_by_lc <- r2_by_site %>%
    group_by(LC) %>%
    summarise(
      R2 = mean(R2),
      .groups = "drop"
    )
  
  overfit_table <- cv_results %>%
    group_by(LC, index) %>%
    summarise(
      train_rmse = first(train_rmse),
      eval_rmse  = first(eval_rmse),
      rmse_gap   = first(rmse_gap),
      rmse_ratio = first(rmse_ratio),
      has_overfit = first(has_overfit),
      .groups = "drop"
    )
  
  r2_by_index <- cv_results %>%
    group_by(LC, index) %>%
    summarise(
      R2_fused    = cor(flux_pred, flux_obs, use = "complete.obs")^2,
      R2_baseline = cor(flux_mean, flux_obs, use = "complete.obs")^2,
      .groups = "drop"
    )
  
  return(list(
    r2_by_site = r2_by_site,
    r2_by_lc = r2_by_lc,
    overfit_table = overfit_table,
    r2_by_index = r2_by_index
  ))
}

##' @title write_bcm_outputs
##' @name write_bcm_outputs
##' @author Yang Gu
##'
##' @param file_prefix Output file prefix.
##' @param flux_cfg Flux-specific configuration list.
##' @param overfit_table Overfitting summary table.
##' @param pdat Prediction output table.
##' @param r2_by_site Site-level R2 table.
##' @param r2_by_lc LC-level R2 table.
##' @param cv_results Cross-validation output table.
##' @param r2_by_index Index-level corrected-vs-baseline R2 table.
##'
##' @description
##' Write BCM outputs to disk, using flux-specific file suffixes.
##'
##' @return
##' No return value.
write_bcm_outputs <- function(file_prefix,
                              flux_cfg,
                              overfit_table,
                              pdat,
                              r2_by_site,
                              r2_by_lc,
                              cv_results,
                              r2_by_index) {
  fwrite(overfit_table, paste0(file_prefix, "_lc1_", flux_cfg$output_tag, "_overfit.csv"))
  fwrite(pdat,          paste0(file_prefix, "_pred_lc1_", flux_cfg$output_tag, ".csv"))
  
  write.csv(
    r2_by_site,
    paste0(file_prefix, "_lc1_", flux_cfg$output_tag, "_site.csv"),
    row.names = FALSE, quote = TRUE
  )
  write.csv(
    r2_by_lc,
    paste0(file_prefix, "_lc1_", flux_cfg$output_tag, "_r2.csv"),
    row.names = FALSE, quote = TRUE
  )
  write.csv(
    cv_results,
    paste0(file_prefix, "_lc1_", flux_cfg$output_tag, "_cv.csv"),
    row.names = FALSE, quote = TRUE
  )
  write.csv(
    r2_by_index,
    paste0(file_prefix, "_lc1_", flux_cfg$output_tag, "_index_improve_r2.csv"),
    row.names = FALSE
  )
}

##' @title run_bcm_pipeline
##' @name run_bcm_pipeline
##' @author Yang Gu
##'
##' @param flux_type Flux type, either "LE" or "NEE".
##' @param raw_response_var Raw ensemble response variable, e.g. "ens01".
##' @param envres_file Path to envres .RData file.
##' @param matched_file Path to matched point-site CSV file.
##' @param output_dir Directory for writing outputs.
##'
##' @description
##' Run the full BCM workflow for a selected flux type using the integrated
##' `envres` dataset and ensemble gap-fill predictions.
##'
##' @return
##' A list containing prediction outputs and evaluation summaries.
run_bcm_pipeline <- function(flux_type,
                             raw_response_var,
                             envres_file,
                             matched_file,
                             output_dir) {
  flux_cfg <- get_flux_config(flux_type)
  response_var <- paste0(raw_response_var, "_residual")
  
  data_obj <- read_close_points_and_envres(
    matched_file = matched_file,
    envres_file = envres_file
  )
  close_points_df <- data_obj$close_points_df
  envres <- data_obj$envres
  
  gfed <- fread(flux_cfg$gfed_file)
  
  envres <- filter_envres_for_flux(envres, flux_cfg)
  envres <- add_gapfill_and_predictors(
    envres = envres,
    gfed = gfed,
    close_points_df = close_points_df,
    raw_response_var = raw_response_var,
    flux_cfg = flux_cfg
  )
  
  split_obj <- split_bcm_data(envres, response_var)
  tenvres <- split_obj$tenvres
  pdat <- split_obj$pdat
  rm(envres)
  gc()
  
  model_obj <- get_bcm_model_components()
  predictors_all <- model_obj$predictors_all
  pred_formula <- model_obj$pred_formula
  lc_param_list <- model_obj$lc_param_list
  default_params <- model_obj$default_params
  nrounds_fixed <- model_obj$nrounds_fixed
  
  tasks <- get_valid_lc_tasks(tenvres)
  
  cv_results <- run_bcm_cv(
    tenvres = tenvres,
    tasks = tasks,
    predictors_all = predictors_all,
    response_var = response_var,
    nrounds_fixed = nrounds_fixed,
    lc_param_list = lc_param_list,
    default_params = default_params,
    flux_cfg = flux_cfg
  )
  
  pdat <- predict_bcm(
    tenvres = tenvres,
    pdat = pdat,
    response_var = response_var,
    pred_formula = pred_formula,
    predictors_all = predictors_all,
    nrounds_fixed = nrounds_fixed,
    lc_param_list = lc_param_list,
    default_params = default_params,
    flux_cfg = flux_cfg
  )
  
  cols_to_keep <- c(
    "utc", "index",
    "flux_resi_pred", "flux_corrected",
    flux_cfg$pred_residual_col,
    flux_cfg$corrected_col
  )
  cols_to_keep <- intersect(cols_to_keep, names(pdat))
  pdat <- pdat[, ..cols_to_keep]
  
  metric_obj <- compute_bcm_metrics(cv_results)
  
  file_prefix <- paste0(output_dir, "/", response_var)
  
  write_bcm_outputs(
    file_prefix = file_prefix,
    flux_cfg = flux_cfg,
    overfit_table = metric_obj$overfit_table,
    pdat = pdat,
    r2_by_site = metric_obj$r2_by_site,
    r2_by_lc = metric_obj$r2_by_lc,
    cv_results = cv_results,
    r2_by_index = metric_obj$r2_by_index
  )
  
  return(list(
    pdat = pdat,
    cv_results = cv_results,
    r2_by_site = metric_obj$r2_by_site,
    r2_by_lc = metric_obj$r2_by_lc,
    overfit_table = metric_obj$overfit_table,
    r2_by_index = metric_obj$r2_by_index
  ))
}