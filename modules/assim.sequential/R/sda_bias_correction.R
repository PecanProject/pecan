#' @description
#' This function helps to extract the covariates based on 
#' date and coordinates from the settings object.
#' @title debias.extract.cov
#' 
#' @param cov.dir character: physical path to the directory contains the time series covariate maps.
#' @param date numeric or character: date that used to find the corresponding covariate file.
#' @param site.info data.frame: data.frame that contains longitude and latitude in its first and second column.
#'
#' @return data.frame: a data frame containing M (rows) of locations across N (columns) variables.
#' 
#' @author Dongchen Zhang
#' @importFrom dplyr %>%
debias.extract.cov <- function (cov.dir, date, site.info) {
  # create terra spatial points.
  pts <- terra::vect(cbind(site.info$Lon, site.info$Lat), crs = "epsg:4326")
  # find which file named by the date.
  cov.file <- list.files(cov.dir, full.names = T)[which(grepl(date, list.files(cov.dir)))]
  # extract covariates.
  cov.df <- terra::extract(x = terra::rast(cov.file), y = pts)[,-1] # remove the first ID column.
  # factorize land cover band.
  if ("LC" %in% colnames(cov.df)) {
    cov.df[,"LC"] <- factor(cov.df[,"LC"])
  }
  # return the extracted data.
  return(cov.df)
}

#' @description
#' This function helps to create the mapping operator that maps between product 
#' names and variable names by locations.
#' @title debias.map.products
#' 
#' @param obs.mean.t list: lists of sites that contain observations for each variable.
#' @param obs_prep list: lists contains basic information for each product of each observation.
#'
#' @return matrix: a matrix containing M (rows) of locations*variables across N (columns) products.
#' 
#' @author Dongchen Zhang
#' @importFrom dplyr %>%
debias.map.products <- function (obs.mean.t, obs_prep) {
  # grab basic info.
  product.names <- names(obs_prep$variables)
  var.names <- unique(obs_prep$variables %>% purrr::map(~.x$var_name) %>% unlist)
  # initialize H matrix.
  H <- matrix(0, nrow = length(obs.mean.t) * length(var.names),
              ncol = length(product.names)) %>% 
    `colnames<-`(product.names) # columns = products.
  # row names = site ids by variable names.
  row.names <- expand.grid(var.names, names(obs.mean.t))
  row.names <- paste0("site.", row.names[,2], ".var.", row.names[,1])
  rownames(H) <- row.names
  # loop over sites.
  for (i in seq_along(obs.mean.t)) {
    obs.site <- obs.mean.t[[i]]
    for (j in seq_along(obs.site)) {
      # update the current row name.
      # by site.id and variable name.
      site.row.name <- paste0("site.", names(obs.mean.t)[i], ".var.", names(obs.site)[j])
      obs.site.var <- obs.site[[j]]
      # assume the first element is product name.
      product <- attributes(obs.site.var)[[1]]
      # fill in H.
      H[site.row.name, product] <- 1
    }
  }
  # return H.
  return(H)
}

#' @description
#' This function helps to correct the forecasts' biases based on 
#' ML (random forest) training on the previous time points.
#' @title sda.bias.correction
#' 
#' @param settings  PEcAn settings object.
#' @param t numeric: the current number of time points (e.g., t=1 for the beginning time point).
#' @param t.start numeric: the user-defined time point to avoid the initial burnin period.
#' @param dates vector: a vector of dates used for extracting covariates through time.
#' @param all.X list: lists of data frame of model forecast from the beginning to the current time points 
#' that has n (ensemble size) rows and n.var (number of variables) times n.site (number of locations) columns.
#' (e.g., 100 ensembles, 4 variables, and 8,000 locations will end up with data.frame of 100 rows and 32,000 columns)
#' @param obs.mean List: lists of date times named by time points, which contains lists of sites named by site ids, 
#' which contains observation means for each state variables of each site for each time point.
#' @param state.interval matrix: containing the upper and lower boundaries for each state variable.
#' @param cov.dir character: physical path to the directory contains the time series covariate maps.
#' @param residual.lag logical: decide if we want to include the lagged 
#' residual (difference in residual between time points) in the ML process.
#' @param py.init R function: R function to initialize the python functions. Default is NULL.
#' the default random forest will be used if `py.init` is NULL.
#'
#' @return list: the current X after the bias-correction; 
#' the ML outputs for each variable; predicted residuals.
#' 
#' @author Dongchen Zhang
#' @importFrom dplyr %>%
sda.bias.correction <- function (settings, 
                                 t, 
                                 t.start, 
                                 dates, 
                                 all.X, 
                                 obs.mean, 
                                 state.interval, 
                                 cov.dir, 
                                 residual.lag = FALSE, 
                                 py.init = NULL) {
  # if t <= t.start.
  if (t <= t.start) {
    PEcAn.logger::logger.info("Please make sure the starting time 
                              point is ealier than the current time point.")
    return(0)
  }
  # grab the site info lists from settings.
  site.info <- settings %>% purrr::map(~.x[['run']] ) %>% 
    purrr::map('site') %>% purrr::map(function(s){
      temp <- as.numeric(c(s$lon, s$lat, s$id))
      names(temp) <- c("Lon", "Lat", "site.id")
      temp
    }) %>% 
    dplyr::bind_rows() %>% 
    as.data.frame()
  obs_prep <- settings$state.data.assimilation$Obs_Prep
  var.names <- unique(obs_prep$variables %>% purrr::map(~.x$var_name) %>% unlist)
  # check if we have generated the covariates file.
  if (!file.exists(file.path(settings$outdir, "debias_cov.rds"))) {
    # extract covariates for all time points.
    cov.ts <- seq_along(dates) %>% 
      furrr::future_map(function(tt){
        debias.extract.cov(cov.dir = cov.dir, 
                           date = dates[tt], 
                           site.info = site.info)
      })
    saveRDS(cov.ts, file = file.path(settings$outdir, "debias_cov.rds"))
  } else {
    cov.ts <- readRDS(file.path(settings$outdir, "debias_cov.rds"))
  }
  # check if we have generated the H file.
  if (!file.exists(file.path(settings$outdir, "debias_H.rds"))) {
    # calculate H matrix for all time points.
    H.ts <- seq_along(obs.mean) %>% 
      furrr::future_map(function(tt){
        debias.map.products(obs.mean[[tt]], obs_prep)})
    saveRDS(H.ts, file = file.path(settings$outdir, "debias_H.rds"))
  } else {
    H.ts <- readRDS(file.path(settings$outdir, "debias_H.rds"))
  }
  # check if we need to calculate the by-product global accuracy to 
  # average the predicted residuals across products.
  # check from t=1 to t=t-1.
  uniq.num.multi.products <- seq_len(t-1) %>% purrr::map(function(tt) {
    apply(H.ts[[tt]], 1, FUN = function(x){length(which(x==1))})
  }) %>% unlist %>% unique()
  # initialize predicted residuals.
  res.pred <- H.ts[[t]] * NA
  # if we have any time for any site that has more than 
  # one products available for the same variable.
  if (any(uniq.num.multi.products > 0)) {
    multi.product <- TRUE
    # initialize the accuracy vector.
    multi.product.accuracy <- rep(NA, ncol(res.pred))
    multi.product.ml <- vector("list", ncol(res.pred))
  } else {
    # else we can use the default residual averaging function.
    multi.product <- FALSE
    multi.product.accuracy <- multi.product.ml <- NULL
  }
  # initialize variable importance.
  var.imp <- list()
  # loop over products.
  for (i in seq_len(ncol(res.pred))) {
    # grab basic info.
    inds <- which(H.ts[[t]][,i] == 1) # which site has observation.
    # skip if there is no observation.
    if (length(inds) == 0) next
    var.name <- unique(strsplit(rownames(res.pred)[inds], split = ".", fixed = T) %>% 
                         purrr::map(function (s) {s[4]}) %>% unlist())
    # convert from site*variable to site.
    inds <- floor(inds/length(var.names)) + 1
    # report progress.
    message(paste0("Processing: ", colnames(res.pred)[i]), " ", var.name)
    # initialize covariate and residual matrix.
    res.i <- X.i <- list()
    # loop over time to find the historical data for training and predicting.
    for (j in t.start:t) {
      # calculate the historical residuals.
      res.i[[j]] <- debias.residual.calc(obs.mean, all.X, j, var.name, colnames(res.pred)[i]) # store
      # calculate forecast means.
      X.i[[j]] <- colMeans(all.X[[j]][,which(grepl(var.name, colnames(all.X[[j]])))])
    }
    # split extractions into training and predicting data sets.
    cov.train <- do.call("rbind", cov.ts[t.start:(t-1)])
    cov.pred <- cov.ts[[t]]; res <- do.call("c", res.i[t.start:(t-1)])
    X.train <- do.call("c", X.i[t.start:(t-1)]); X.pred <- X.i[[t]]
    # if we want to include the residual lag.
    if (residual.lag) {
      res.lag.i <- debias.res.lag.calc(res.i)
      res.lag.train <- do.call("c", res.lag.i[t.start:(t-1)])
      if (all(is.na(res.lag.train))) {
        res.lag.train <- res.lag.pred <- NULL
        cov.names <- c(colnames(cov.train), "X")
      } else {
        res.lag.pred <- res.lag.i[[t]]
        cov.names <- c(colnames(cov.train), "X", "res.lag")
      }
    } else {
      res.lag.train <- res.lag.pred <- NULL
      cov.names <- c(colnames(cov.train), "X")
    }
    # assemble extractions.
    # prepare training data.
    dat.train <- list(cov.train, X.train, res.lag.train, res)
    dat.train <- do.call(cbind, dat.train[!sapply(dat.train, is.null)])
    dat.train <- dat.train[stats::complete.cases(dat.train),]
    colnames(dat.train) <- c(cov.names, "res")
    # skip if there is no training data.
    if (dim(dat.train)[1] <= 1) next
    # if we have examples that have more than one products for single variable.
    if (multi.product) {
      # calculate the by-product out-of-sample accuracy of residual predictions.
      multi.accuracy.res <- debias.train.accuracy(pred.name = "res", 
                                                  cov.names = cov.names, 
                                                  dat.train = dat.train, 
                                                  var.name = var.name, 
                                                  py.init = py.init)
      multi.product.accuracy[i] <- multi.accuracy.res$accuracy
      multi.product.ml[[i]] <- multi.accuracy.res$var.imp
    }
    # prepare predicting data.
    dat.pred <- list(cov.pred, X.pred, res.lag.pred)
    dat.pred <- do.call(cbind, dat.pred[!sapply(dat.pred, is.null)])
    pred.complete.inds <- which(stats::complete.cases(dat.pred))
    # grab index that satisfy both non-NA and inds conditions.
    pred.complete.inds <- intersect(pred.complete.inds, inds)
    dat.pred <- dat.pred[pred.complete.inds,]
    colnames(dat.pred) <- cov.names
    if (dim(dat.pred)[1] == 0) next
    # ML predictions.
    debias.out <- debias.ML(pred.name = "res", cov.names = cov.names, 
                            dat.train = dat.train, dat.pred = dat.pred, 
                            var.name = var.name, py.init = py.init)
    # store into the matrix.
    complete.site.ids <- site.info$site.id[pred.complete.inds]
    rows <- paste0("site.", complete.site.ids, ".var.", var.name)
    res.pred[rows, i] <- debias.out$prediction
    var.imp[[i]] <- debias.out$var.imp
  }
  # bias corrections.
  X <- all.X[[t]] # grab the current forecasts.
  # loop over res.pred matrix.
  for (i in seq_len(nrow(res.pred))) {
    inds <- which(!is.na(res.pred[i,])) # find valid residual predictions.
    # calculate residuals based on different conditions.
    if (length(inds) == 0) {
      next
    } else if (length(inds) == 1) {
      residual <- res.pred[i, inds]
    } else if (length(inds) > 1) {
      residual <- debias.average(res.pred[i, inds], multi.product.accuracy[inds])
    }
    # correction.
    site.id <- strsplit(rownames(res.pred)[i], split = ".", fixed = T)[[1]][2]
    var.name <- strsplit(rownames(res.pred)[i], split = ".", fixed = T)[[1]][4]
    col.ind <- which(attributes(X)$Site == site.id & grepl(var.name, colnames(X)))
    X[,col.ind] <- X[,col.ind] - residual
  }
  # map forecasts towards the prescribed variable boundaries.
  for(i in 1:ncol(X)){
    int.save <- state.interval[which(startsWith(colnames(X)[i], var.names)),]
    X[X[,i] < int.save[1],i] <- int.save[1]
    X[X[,i] > int.save[2],i] <- int.save[2]
  }
  return(list(X = X, var.imp = var.imp, res = res.pred, multi.product.accuracy = multi.product.accuracy, multi.product.ml = multi.product.ml))
}

#' @description
#' This function helps to average the predicted residuals based on certain criteria.
#' The default function is averaging.
#' TODO: we will be averaging residuals based on ML precision for each residual prediction.
#' @title debias.average
#' 
#' @param residuals vector: predicted residuals across products.
#' @param by_product_accuracy vector: the out-of-sample RMSE for each product.
#'
#' @return numeric: an averaged residual.
#' 
#' @author Dongchen Zhang
#' @importFrom dplyr %>%
debias.average <- function (residuals, by_product_accuracy = NULL) {
  if (is.null(by_product_accuracy)) {
    # calculate mean residual as default.
    return(mean(residuals))
  } else {
    # calculate weights based on accuracy.
    weights <- c()
    for (i in seq_along(by_product_accuracy)) {
      weights <- c(weights, 1/by_product_accuracy[i]^2) # in the future: try squaring it.
    }
    weights <- weights/sum(weights)
    # return residuals.
    return(sum(residuals*weights))
  }
}

#' @description
#' This function helps to calculate the lagged residual error time series.
#' @title debias.res.lag.calc
#' 
#' @param res.ts list: lists of residuals at each time point.
#'
#' @return list: lists of lagged residuals between time points.
#' 
#' @author Dongchen Zhang
#' @importFrom dplyr %>%
debias.res.lag.calc <- function (res.ts) {
  # if res.ts has length less than 2.
  if (length(res.ts) < 2) {
    PEcAn.logger::logger.info("The current residual list has length less than 2.")
    return(0)
  }
  res.lag.ts <- res.ts %>% purrr::map(function(res){res*NA}) # initialize list.
  for (i in rev(seq_along(res.ts))) {
    if (i > 1) { # only do the calculation when t>1.
      if (!is.null(res.ts[[i]]) & !is.null(res.ts[[i-1]])) {
        res.lag.ts[[i]] <- res.ts[[i]] - res.ts[[i-1]]
      }
    } else {
      break
    }
  }
  return(res.lag.ts)
}

#' @description
#' This function helps to correct the forecasts' biases based on 
#' ML (random forest) training on the previous time points.
#' @title sda.bias.correction
#' 
#' @param pred.name  character: the name for the predictive variable.
#' @param cov.names character: the name for the covariates.
#' @param dat.train data.frame: data frame containing associated covariates and 
#' predictive variable for ML training.
#' @param dat.pred data.frame: data frame containing associated covariates for prediction.
#' @param var.name character: variable name to be predicted.
#' @param py.init R function: R function to initialize the python functions. Default is NULL.
#' the default random forest will be used if `py.init` is NULL.
#'
#' @return list: the ML predicted residuals and other ML outputs. 
#' 
#' @author Dongchen Zhang
#' @importFrom dplyr %>%
debias.ML <- function (pred.name, cov.names, dat.train, dat.pred, var.name, py.init) {
  if (is.null(py.init)) {
    # random forest training.
    formula <- stats::as.formula(paste(pred.name, "~", paste(cov.names, collapse = " + ")))
    model <- randomForest::randomForest(formula,
                                        data = dat.train,
                                        ntree = 1000,
                                        na.action = stats::na.omit,
                                        keep.forest = TRUE,
                                        importance = TRUE)
    var.imp <- randomForest::importance(model)
    # predict residuals for the current time point.
    res.pred <- stats::predict(model, dat.pred)
    output <- list(prediction = res.pred, var.imp = var.imp)
  } else {
    # if we have prescribed python script to use.
    # load python functions.
    py <- py.init()
    # using functions from the python script.
    # training.
    fi_ret <- py$train_full_model(
      name = as.character(var.name), # current variable name.
      X = as.matrix(dat.train[,-length(dat.train)]), # covariates + previous forecast means.
      y = as.numeric(dat.train[[pred.name]]), # residuals.
      feature_names = colnames(dat.train[,-length(dat.train)])
    )
    # predicting.
    res.pred <- py$predict_residual(as.character(var.name), as.matrix(dat.pred))
    # store model outputs.
    # weights.
    w_now <- try(py$get_model_weights(as.character(var.name)), silent = TRUE)
    w_now <- min(max(as.numeric(w_now), 0), 1)
    w_named <- c(KNN = w_now, TREE = 1 - w_now)
    # var importance.
    fi_ret <- tryCatch(reticulate::py_to_r(fi_ret), error = function(e) fi_ret)
    fn <- as.character(unlist(fi_ret[["names"]], use.names = FALSE))
    fv <- as.numeric(unlist(fi_ret[["importances"]], use.names = FALSE)) %>% purrr::set_names(fn)
    output <- list(prediction = res.pred, weights = w_named, var.imp = fv)
  }
  return(output)
}

#' @description
#' This function helps to calculate the out-of-sample accuracy of residual predictions.
#' @title debias.train.accuracy
#' 
#' @param pred.name  character: the name for the predictive variable.
#' @param cov.names character: the name for the covariates.
#' @param dat.train data.frame: data frame containing associated covariates and 
#' predictive variable for ML training.
#' @param var.name character: variable name to be predicted.
#' @param py.init R function: R function to initialize the python functions. Default is NULL.
#' the default random forest will be used if `py.init` is NULL.
#' @param ratio numeric from 0 to 1: define the ratio of samples used for training.
#' The rest samples will be used for calculating the out-of-sample accuracy.
#' Default is 0.8.
#'
#' @return list: the variable importance of the ML and RMSE of the out-of-sample predictions. 
#' 
#' @author Dongchen Zhang
#' @importFrom dplyr %>%
debias.train.accuracy <- function (pred.name, cov.names, dat.train, var.name, py.init, ratio = 0.8) {
  # sampling records by the predefined ratio
  samples <- seq_len(nrow(dat.train))
  inds.train <- sample(samples, size = ceiling(length(samples)*ratio), replace = FALSE)
  inds.pred <- which(!samples %in% inds.train)
  # splitting samples.
  dat.train.split <- dat.train[inds.train,]
  dat.pred.split <- dat.train[inds.pred,]
  # remove the residual column.
  dat.pred.split <- dat.pred.split[,-which(colnames(dat.pred.split) == pred.name)]
  # ML.
  res.pred <- debias.ML(pred.name = pred.name, cov.names = cov.names, 
                        dat.train = dat.train.split, dat.pred = dat.pred.split, 
                        var.name = var.name, py.init = py.init)
  # calculate accuracy.
  res.rmse <- Metrics::rmse(actual = dat.train[inds.pred, pred.name], 
                            predicted = res.pred$prediction)
  return(list(accuracy = res.rmse, var.imp = res.pred$var.imp))
}

#' @description
#' This function helps to calculate the residual error for a certain time point and variable.
#' @title debias.residual.calc
#' 
#' @param obs.mean List: lists of date times named by time points, which contains lists of sites named by site ids, 
#' which contains observation means for each state variables of each site for each time point.
#' @param all.X list: lists of data frame of model forecast from the beginning to the current time points 
#' that has n (ensemble size) rows and n.var (number of variables) times n.site (number of locations) columns.
#' (e.g., 100 ensembles, 4 variables, and 8,000 locations will end up with data.frame of 100 rows and 32,000 columns).
#' @param t numeric: the current number of time points (e.g., t=1 for the beginning time point).
#' @param var.name character: variable name to be predicted.
#' @param data.source character: product name of the data.
#' @return list: lists of residuals between forecasts and observations across at time t.
#' 
#' @author Dongchen Zhang
#' @importFrom dplyr %>%
debias.residual.calc <- function (obs.mean, all.X, t, var.name, data.source) {
  # grab observation at t for var.name.
  obs.t.var <- obs.mean[[t]] %>% 
    purrr::map(function(obs){
      v.att <- obs %>% purrr::map(function(o) {
        attributes(o)[[1]]
      }) %>% unlist
      v.ind <- which(names(v.att) == var.name & v.att == data.source)
      if (length(v.ind) == 1) {
        return(obs[[v.ind]])
      } else {
        return(NA)
      }
    }) %>% unlist
  # grab forecasts at t for var.name.
  X.t.var <- colMeans(all.X[[t]][,which(grepl(var.name, colnames(all.X[[t]])))])
  res <- X.t.var - obs.t.var
  return(res)
}