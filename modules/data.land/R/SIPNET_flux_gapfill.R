#' @description
#' This function reads the site information table and returns the selected
#' site IDs for downstream modeling.
#'
#' @title read_site_ids
#'
#' @param site.dir character: path to the site information table.
#' @param n_sites numeric: number of sites to keep from the site table,
#' the total number of sites is 241.
#'
#' @return character vector of selected site IDs. Example: 'Site ID' CA-ARB
#'
#' @author Yang Gu
read_site_ids <- function(site.dir, n_sites = 40) {
  fluxnet.df <- data.table::fread(site.dir) |>
    tibble::as_tibble()
  
  site_ids <- fluxnet.df$`Site ID`[1:n_sites] |>
    unique()
  
  return(site_ids)
}

#' @description
#' This function reads the predictor/flux dataset and keeps only rows
#' corresponding to the selected site IDs.
#'
#' @title read_pred_data
#'
#' @param pred.var.dir character: path to the predictor/flux data file.
#' @param site_ids character: vector of site IDs to retain.
#'
#' @return data.frame containing filtered predictor/flux data.
#'
#' @author Yang Gu
read_pred_data <- function(pred.var.dir, site_ids) {
  resimet.df <- data.table::fread(pred.var.dir) |>
    tibble::as_tibble() |>
    dplyr::mutate(is_night_suncalc = as.integer(is_night_suncalc))
  
  resimet.df <- resimet.df |>
    dplyr::filter(Site_ID %in% site_ids)
  
  return(resimet.df)
}

#' @description
#' This function calculates site-specific missing-value percentages for
#' candidate predictor variables and returns a long-format table of retained
#' predictors for each site.
#'
#' @title build_sitecov_df
#'
#' @param resimet.df data.frame: predictor/flux dataset filtered to target sites.
#' @param vars_to_check character: vector of candidate predictor variable names.
#' @param outdir character: optional output directory to save the missing summary.
#' Default is NULL.
#' @param missing_threshold numeric: maximum allowed missing percentage for a
#' predictor to be retained at a site. Default is 40.
#'
#' @return list containing:
#' \describe{
#'   \item{missing_summary}{data frame of missing-value summary by site and variable.}
#'   \item{sitecov.df}{long-format data frame of retained predictors by site.}
#' }
#'
#' @author Yang Gu
build_sitecov_df <- function(resimet.df,
                             vars_to_check,
                             outdir = NULL,
                             missing_threshold = 40) {
  missing_summary <- resimet.df |>
    dplyr::select(Site_ID, dplyr::all_of(vars_to_check)) |>
    tidyr::pivot_longer(
      cols = -Site_ID,
      names_to = "variable",
      values_to = "value"
    ) |>
    dplyr::group_by(Site_ID, variable) |>
    dplyr::summarise(
      total_obs = dplyr::n(),
      missing_count = sum(is.na(value)),
      missing_pct = missing_count / total_obs * 100,
      .groups = "drop"
    ) |>
    dplyr::arrange(Site_ID, dplyr::desc(missing_pct))
  
  if (!is.null(outdir)) {
    if (!file.exists(outdir)) {
      dir.create(outdir, recursive = TRUE)
    }
    data.table::fwrite(
      missing_summary,
      file.path(outdir, "summary.csv")
    )
  }
  
  high_staying_vars <- missing_summary |>
    dplyr::filter(missing_pct < missing_threshold) |>
    dplyr::group_by(Site_ID) |>
    dplyr::summarise(
      filtered_variables = list(variable),
      .groups = "drop"
    )
  
  sitecov.df <- high_staying_vars |>
    tidyr::unnest(filtered_variables)
  
  return(list(
    missing_summary = missing_summary,
    sitecov.df = sitecov.df
  ))
}

#' @description
#' This internal helper function fits a site-specific XGBoost model using
#' k-fold CV. It evaluates model performance with fold-level
#' training and validation R2, and fits a final model using the full
#' site training dataset if the average validation R2 exceeds the
#' specified threshold.
#'
#' @title fit_site_xgb_cv
#'
#' @param subdf data.frame: a site-specific training data frame containing
#' the response variable and predictor variables.
#' @param feats character: a character vector of predictor variable names used
#' for model fitting.
#' @param site_name character: site identifier used for progress reporting.
#' @param response_var character: the name of the response variable column in
#' `subdf`.
#' @param nfolds numeric: number of folds used in CV.
#' @param params list: a list of XGBoost model parameters passed to
#' `xgboost::xgb.train`.
#' @param nrounds numeric: number of boosting rounds used in XGBoost training.
#' @param r2_threshold numeric: minimum average validation R2 required
#' to fit the final model.
#'
#' @return list containing:
#' \describe{
#'   \item{r2}{average validation R2 across folds.}
#'   \item{model}{the final fitted XGBoost model, or NULL if the site does not meet the threshold.}
#'   \item{folds_df}{a data frame of fold-level training and validation R2.}
#' }
#'
#' @author Yang Gu
fit_site_xgb_cv <- function(subdf,
                            feats,
                            site_name,
                            response_var,
                            nfolds,
                            params,
                            nrounds,
                            r2_threshold) {
  
  # Return empty results if the site has too few samples or no predictors
  if (nrow(subdf) < nfolds || length(feats) == 0) {
    return(list(
      r2 = NA_real_,
      model = NULL,
      folds_df = tibble::tibble()
    ))
  }
  
  # Randomly assign observations to CV folds
  set.seed(123)
  folds <- sample(rep(seq_len(nfolds), length.out = nrow(subdf)))
  
  # Initialize containers for fold-level performance metrics
  r2s <- numeric(nfolds)
  r2s_train <- numeric(nfolds)
  folds_df <- vector("list", nfolds)
  
  # Loop over folds
  for (i in seq_len(nfolds)) {
    # Split training and validation subsets
    tr <- subdf[folds != i, , drop = FALSE]
    te <- subdf[folds == i, , drop = FALSE]
    
    # Construct XGBoost training matrix
    dtrain <- xgboost::xgb.DMatrix(
      data = as.matrix(dplyr::select(tr, dplyr::all_of(feats))),
      label = tr[[response_var]],
      missing = NA
    )
    
    # Fit fold-specific XGBoost model
    mdl <- xgboost::xgb.train(
      params = params,
      data = dtrain,
      nrounds = nrounds,
      verbose = 0
    )
    
    # Construct validation matrix
    dvalid <- xgboost::xgb.DMatrix(
      data = as.matrix(dplyr::select(te, dplyr::all_of(feats))),
      missing = NA
    )
    
    # Predict on validation and training data
    pred_valid <- stats::predict(mdl, dvalid)
    pred_train <- stats::predict(mdl, dtrain)
    
    # Compute denominator for R2
    denom_valid <- sum((te[[response_var]] - mean(te[[response_var]], na.rm = TRUE))^2, na.rm = TRUE)
    denom_train <- sum((tr[[response_var]] - mean(tr[[response_var]], na.rm = TRUE))^2, na.rm = TRUE)
    
    # Compute validation R2
    r2_valid <- if (denom_valid > 0) {
      1 - sum((te[[response_var]] - pred_valid)^2, na.rm = TRUE) / denom_valid
    } else {
      NA_real_
    }
    
    # Compute training R2
    r2_train <- if (denom_train > 0) {
      1 - sum((tr[[response_var]] - pred_train)^2, na.rm = TRUE) / denom_train
    } else {
      NA_real_
    }
    
    # Store fold-level metrics
    r2s[i] <- r2_valid
    r2s_train[i] <- r2_train
    
    folds_df[[i]] <- tibble::tibble(
      fold = i,
      R2_train = r2_train,
      R2_valid = r2_valid
    )
  }
  
  # Print fold-level validation performance for the current site
  message(sprintf(
    "[Site %s] Fold Valid R2: %s",
    site_name,
    paste(round(r2s, 3), collapse = ", ")
  ))
  
  # Compute average validation R2 across folds
  avg_r2 <- mean(r2s, na.rm = TRUE)
  final_mdl <- NULL
  
  # Fit final model using all site data if the site passes the threshold
  if (!is.na(avg_r2) && avg_r2 >= r2_threshold) {
    final_mdl <- xgboost::xgb.train(
      params = params,
      data = xgboost::xgb.DMatrix(
        data = as.matrix(dplyr::select(subdf, dplyr::all_of(feats))),
        label = subdf[[response_var]],
        missing = NA
      ),
      nrounds = nrounds,
      verbose = 0
    )
  }
  
  # Return site-level summary results
  list(
    r2 = avg_r2,
    model = final_mdl,
    folds_df = dplyr::bind_rows(folds_df)
  )
}

#' @description
#' Perform site-level XGBoost gap-filling with cross-validation for
#' eddy-covariance flux data. For each ensemble iteration, the function
#' samples high-quality observations (QC = 0) within each site, fits a
#' site-specific XGBoost model using k-fold cross-validation, predicts
#' missing or low-quality flux values, and exports both gap-filled outputs
#' and fold-level CV diagnostics.
#'
#' @title xgb_gapfill
#'
#' @param site.dir character. Path to the site information table.
#' @param pred.var.dir character. Path to the predictor/flux data file.
#' @param flux_var character. Flux type to be gap-filled. Currently supports
#' `"LE"` and `"NEE"`.
#' @param outdir character. Output directory for exported CSV files.
#' @param vars_to_check character vector. Candidate predictor variables used
#' to build site-specific predictor sets.
#' @param n_sites numeric. Number of sites to keep from the site table.
#' @param missing_threshold numeric. Maximum allowed percentage of missingness
#' when selecting predictors for each site.
#' @param nfolds numeric. Number of folds used in cross-validation. Default is 10.
#' @param nrounds numeric. Number of boosting rounds for XGBoost training.
#' Default is 200.
#' @param nens numeric. Number of ensemble iterations. Default is 25.
#' @param nsamp numeric. Proportion of QC = 0 observations sampled within each
#' site for training in each ensemble. Default is 0.95.
#' @param params list. XGBoost model parameters.
#' @param r2_threshold numeric. Minimum average validation R2 required to keep
#' a site in the exported gap-filled output.
#' @param cores numeric. Number of CPUs used for parallel site-level fitting.
#' @param overwrite logical. Whether existing output files should be overwritten.
#'
#' @return A list of length `nens`. Each element corresponds to one ensemble
#' iteration and contains:
#' \describe{
#'   \item{cv_results}{data frame of site-level average CV R2.}
#'   \item{filled_data}{data frame of the full input dataset with merged gap-filled values.}
#'   \item{exported_data}{data frame of filtered gap-filled results for well-performing sites only.}
#'   \item{folds_all}{data frame of fold-level CV diagnostics across sites.}
#' }
#'
#' @author Yang Gu
#' @importFrom foreach %dopar%
#' @export
xgb_gapfill <- function(site.dir,
                        pred.var.dir,
                        flux_var,
                        outdir,
                        vars_to_check = c(
                          "TA_F","SW_IN_F","SW_DIF","LW_IN_JSB","VPD_F","PA_F","P_F",
                          "RH","NETRAD","PPFD_IN","PPFD_DIF","TS_F_MDS_1","LW_IN_F",
                          "TS_F_MDS_3","TS_F_MDS_5","SWC_F_MDS_1","NDVI","EVI",
                          "sin_doy","cos_doy","SWC_F_MDS_3","SWC_F_MDS_5"
                        ),
                        n_sites = 40,
                        missing_threshold = 40,
                        nfolds = 10,
                        nrounds = 200,
                        nens = 25,
                        nsamp = 0.95,
                        params = list(
                          objective = "reg:squarederror",
                          eta = 0.13,
                          max_depth = 13,
                          subsample = 0.9,
                          colsample_bytree = 0.9
                        ),
                        r2_threshold = 0.6,
                        cores = max(1L, parallel::detectCores() - 1L),
                        overwrite = TRUE) {
  
  if (!requireNamespace("xgboost", quietly = TRUE)) {
    stop("Package 'xgboost' is required for cv_xgb_site_cv_gapfill() but is not installed.")
  }
  if (!requireNamespace("doParallel", quietly = TRUE)) {
    stop("Package 'doParallel' is required for parallel execution but is not installed.")
  }
  if (!requireNamespace("foreach", quietly = TRUE)) {
    stop("Package 'foreach' is required for parallel execution but is not installed.")
  }
  
  # Read target site IDs
  site_ids <- read_site_ids(
    site.dir = site.dir,
    n_sites = n_sites
  )
  
  # Read predictor and flux data for selected sites
  resimet.df <- read_pred_data(
    pred.var.dir = pred.var.dir,
    site_ids = site_ids
  )
  
  # Build site-specific predictor table
  sitecov_res <- build_sitecov_df(
    resimet.df = resimet.df,
    vars_to_check = vars_to_check,
    outdir = outdir,
    missing_threshold = missing_threshold
  )
  sitecov.df <- sitecov_res$sitecov.df
  
  # Map flux type to variable names
  flux_var <- toupper(flux_var)
  if (flux_var == "LE") {
    response_var <- "LE_F_MDS"
    qc_var <- "LE_F_MDS_QC"
    gapfill_var <- "LE_gapfill"
  } else if (flux_var == "NEE") {
    response_var <- "NEE_CUT_USTAR50"
    qc_var <- "NEE_CUT_USTAR50_QC"
    gapfill_var <- "NEE_gapfill"
  } else {
    stop("Unsupported flux_var. Currently only 'LE' and 'NEE' are supported.")
  }
  
  # Validate required columns in input data
  required_cols <- c("Site_ID", "TIMESTAMP_START", response_var, qc_var)
  missing_cols <- setdiff(required_cols, names(resimet.df))
  if (length(missing_cols) > 0) {
    stop(
      sprintf(
        "Missing required columns in resimet.df: %s",
        paste(missing_cols, collapse = ", ")
      )
    )
  }
  if (!file.exists(outdir)) {
    dir.create(outdir, recursive = TRUE)
  }
  
  # Filter high-quality observations for training (QC = 0)
  data_good <- resimet.df |>
    dplyr::filter(.data[[qc_var]] == 0)
  
  ensemble_results <- vector("list", nens)
  
  # Loop over ensemble members
  for (m in seq_len(nens)) {
    message(sprintf("Ensemble %d/%d", m, nens))
    
    # Stratified Sampling: Sample 95% from within each site for training.
    train_df <- data_good |>
      dplyr::group_by(Site_ID) |>
      dplyr::slice_sample(prop = nsamp) |>
      dplyr::ungroup()
    
    # Build site-specific training datasets
    site_dfs <- lapply(site_ids, function(site) {
      feats0 <- sitecov.df |>
        dplyr::filter(Site_ID == site) |>
        dplyr::pull(filtered_variables) |>
        unique()
      
      feats <- unique(c(feats0, "is_night_suncalc"))
      feats <- intersect(feats, names(train_df))
      
      df <- train_df |>
        dplyr::filter(Site_ID == site) |>
        dplyr::select(
          TIMESTAMP_START,
          Site_ID,
          dplyr::all_of(response_var),
          dplyr::all_of(feats)
        ) |>
        tibble::as_tibble()
      
      attr(df, "features") <- feats
      df
    })
    names(site_dfs) <- site_ids
    
    # Parallel model fitting and gap-filling by site   
    use_cores <- min(as.numeric(cores), length(site_ids))
    cl <- parallel::makeCluster(use_cores, type = "PSOCK")
    doParallel::registerDoParallel(cl)
    results <- tryCatch({
      foreach::foreach(
        site = site_ids,
        .packages = c("dplyr", "xgboost", "tibble"),
        .combine = dplyr::bind_rows,
        .errorhandling = "pass"
      ) %dopar% {
        tryCatch({
          df <- site_dfs[[site]]
          feats <- attr(df, "features")
          
          # Fit XGBoost model with CV
          fit <- fit_site_xgb_cv(
            subdf = df,
            feats = feats,
            site_name = site,
            response_var = response_var,
            nfolds = nfolds,
            params = params,
            nrounds = nrounds,
            r2_threshold = r2_threshold
          )
          
          # Identify gap-fill candidates
          gap_cands <- resimet.df |>
            dplyr::filter(
              Site_ID == site,
              is.na(.data[[response_var]]) | .data[[qc_var]] != 0
            ) |>
            dplyr::select(TIMESTAMP_START, Site_ID, dplyr::all_of(feats)) |>
            tibble::as_tibble()
          
          pd <- gap_cands
          
          # Gap fill flux if model is valid
          if (!is.null(fit$model) && nrow(pd) > 0) {
            dtest <- xgboost::xgb.DMatrix(
              data = as.matrix(dplyr::select(pd, dplyr::all_of(feats))),
              missing = NA
            )
            pd[[gapfill_var]] <- stats::predict(fit$model, dtest)
            pd$GF_QC <- 0L
          } else if (nrow(pd) > 0) {
            pd[[gapfill_var]] <- NA_real_
            pd$GF_QC <- 1L
          }
          
          # Return site-level results
          tibble::tibble(
            Site_ID = site,
            Avg_R2 = fit$r2,
            pred_df = list(pd),
            folds_df = list(dplyr::mutate(fit$folds_df, Site_ID = site))
          )
        }, error = function(e) {
          message(sprintf("[Site %s] ERROR: %s", site, conditionMessage(e)))
          tibble::tibble(
            Site_ID = site,
            Avg_R2 = NA_real_,
            pred_df = list(NULL),
            folds_df = list(tibble::tibble())
          )
        })
      }
    }, error = function(e) {
      message(sprintf("Parallel loop error in ensemble %d: %s", m, conditionMessage(e)))
      NULL
    }, finally = {
      parallel::stopCluster(cl)
      foreach::registerDoSEQ()
    })
    
    # Skip ensemble if no results
    if (is.null(results) || nrow(results) == 0) {
      ensemble_results[[m]] <- NULL
      next
    }
    
    # Combine gap-fill predictions across sites
    pred_list <- results$pred_df
    gap_all <- dplyr::bind_rows(pred_list)
    
    # Merge predictions back to full dataset
    resimet_filled <- resimet.df |>
      dplyr::left_join(gap_all, by = c("Site_ID", "TIMESTAMP_START")) |>
      dplyr::mutate(
        !!gapfill_var := dplyr::if_else(
          !is.na(.data[[gapfill_var]]),
          .data[[gapfill_var]],
          .data[[response_var]]
        ),
        GF_QC = dplyr::if_else(is.na(GF_QC), 1L, GF_QC)
      )
    
    # Extract CV performance
    cv_results <- results |>
      dplyr::select(Site_ID, Avg_R2)
    
    # Select well-performing sites
    good_sites <- cv_results |>
      dplyr::filter(!is.na(Avg_R2) & Avg_R2 >= r2_threshold) |>
      dplyr::pull(Site_ID)
    
    # Subset final output for good sites
    sub.df <- resimet_filled |>
      dplyr::filter(Site_ID %in% good_sites) |>
      dplyr::select(
        Site_ID,
        TIMESTAMP_START,
        dplyr::all_of(response_var),
        dplyr::all_of(gapfill_var),
        GF_QC,
        dplyr::all_of(qc_var)
      )
    
    # Aggregate fold-level CV diagnostics
    folds_all <- dplyr::bind_rows(results$folds_df) |>
      dplyr::mutate(ensemble_id = m)
    
    ens_tag <- sprintf("ens%02d", m)
    flux_tag <- tolower(flux_var)
    
    data_path <- file.path(outdir, sprintf("%s_gfed_dat_%s.csv", flux_tag, ens_tag))
    folds_path <- file.path(outdir, sprintf("%s_gfed_cv_folds_%s.csv", flux_tag, ens_tag))
    
    if (overwrite || !file.exists(data_path)) {
      data.table::fwrite(sub.df, data_path)
    }
    
    if (overwrite || !file.exists(folds_path)) {
      data.table::fwrite(folds_all, folds_path)
    }
    
    # Store ensemble results
    ensemble_results[[m]] <- list(
      cv_results = cv_results,
      filled_data = resimet_filled,
      exported_data = sub.df,
      folds_all = folds_all
    )
    
    rm(results, pred_list, gap_all, resimet_filled, cv_results, good_sites,
       sub.df, folds_all, site_dfs, train_df)
    gc(FALSE)
  }
  
  return(ensemble_results)
}