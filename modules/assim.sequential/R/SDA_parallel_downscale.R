#' @description
#' This function helps to stack target data layers from various GeoTIFF maps (with different extents, CRS, and resolutions) to a single map.
#' @title stack_covariates_2_geotiff
#' 
#' @param outdir character: the output directory where the stacked GeoTIFF file will be generated.
#' @param year numeric: the year of when the covariates are stacked.
#' @param base.map.dir character: path to the GeoTIFF file within which the extents and CRS will be used to generate the final map.
#' @param cov.tif.file.list list: a list contains sub-lists with each including path to the corresponding map and the variables to be extracted (e.g., list(LC = list(dir = "path/to/landcover.tiff", var.name = "LC")).
#' @param normalize boolean: decide if we want to normalize each data layer, the default is TRUE.
#' @param cores numeric: how many CPus to be used in the calculation, the default is the total CPU number you have.
#'
#' @return path to the exported GeoTIFF file.
#' 
#' @export
#' 
#' @author Dongchen Zhang
#' @importFrom foreach %dopar%
stack_covariates_2_geotiff <- function(outdir, year, base.map.dir, cov.tif.file.list, normalize = T, cores = parallel::detectCores()) {
  # create the folder if it doesn't exist.
  if (!file.exists(outdir)) {
    dir.create(outdir)
  }
  # parallel loop.
  # register parallel nodes.
  if (cores > length(cov.tif.file.list)) {
    cores <- length(cov.tif.file.list)
  }
  cl <- parallel::makeCluster(as.numeric(cores))
  doSNOW::registerDoSNOW(cl)
  #progress bar.
  pb <- utils::txtProgressBar(min=1, max=length(cov.tif.file.list), style=3)
  progress <- function(n) utils::setTxtProgressBar(pb, n)
  opts <- list(progress=progress)
  # foreach loop.
  f <- NULL
  paths <- foreach::foreach(f = cov.tif.file.list, 
                            .packages=c("Kendall", "terra"),
                            .options.snow=opts) %dopar% {
                              # load the base map.
                              base.map <- terra::rast(base.map.dir)
                              # read geotif file.
                              temp.rast <- terra::rast(f$dir)
                              # normalize.
                              if (normalize & !"LC" %in% f$var.name) {
                                nx <- terra::minmax(temp.rast)
                                temp.rast <- (temp.rast - nx[1,]) / (nx[2,] - nx[1,])
                              }
                              # set name to layers if we set it up in advance.
                              # otherwise the original layer name will be used.
                              if (!is.null(f$var.name)) {
                                names(temp.rast) <- f$var.name
                              }
                              # raster operations.
                              if (! terra::crs(base.map) == terra::crs(temp.rast)) {
                                terra::crs(temp.rast) <- terra::crs(base.map)
                              }
                              if (! terra::ext(base.map) == terra::ext(temp.rast)) {
                                temp.rast <- terra::crop(temp.rast, base.map)
                              }
                              if (! all(c(nrow(base.map) == nrow(temp.rast), ncol(base.map) == ncol(temp.rast)))) {
                                temp.rast <- terra::resample(temp.rast, base.map)
                              }
                              # write the raster into disk.
                              file.name <- paste0(f$var.name, collapse = "_")
                              path <- file.path(outdir, paste0(file.name, ".tiff"))
                              terra::writeRaster(temp.rast, path)
                              return(path)
                            } %>% unlist
  # stop parallel.
  parallel::stopCluster(cl)
  foreach::registerDoSEQ()
  gc()
  # combine rasters.
  all.rast <- terra::rast(paths)
  # write all covariates into disk.
  terra::writeRaster(all.rast, file.path(outdir, paste0("covariates_", year, ".tiff")), overwrite = T)
  # remove previous tiff files.
  unlink(paths)
  # return results.
  return(file.path(outdir, paste0("covariates_", year, ".tiff")))
}

#' @description
#' convert settings to geospatial points in terra.
#' @title pecan_settings_2_pts
#' 
#' @param settings PEcAn settings: either a character that points to the settings or shape file or the actual pecan settings object will be accepted.
#'
#' @return terra spatial points object.
#' 
#' @author Dongchen Zhang
pecan_settings_2_pts <- function(settings) {
  if (is.character(settings)) {
    # if it's shapefile.
    if (grepl(".shp", settings)) {
      return(terra::vect(settings))
    }
    # read settings.
    settings <- PEcAn.settings::read.settings(settings)
  }
  # grab lat/lon.
  site.locs <- settings$run %>% purrr::map('site') %>% 
    purrr::map_dfr(~c(.x[['lon']],.x[['lat']]) %>% as.numeric)%>% 
    t %>% `colnames<-`(c("Lon","Lat")) %>% as.data.frame()
  # convert lat/lon to terra::vect.
  pts <- terra::vect(site.locs, geom = c("Lon", "Lat"), crs = "EPSG:4326")
  return(pts)
}

#' @description
#' This function helps to build the data frame (pixels by data columns) for only vegetated pixels to improve the efficiency.
#' Note that the `LC` field using the `MODIS land cover` observations (MCD12Q1.061) must be supplied in the covariates to make this function work.
#' @title stack_covariates_2_df
#' 
#' @param rast.dir character: a character that points to the covariates raster file generated by the `stack_covariates_2_geotiff` function.
#' @param cores numeric: how many CPus to be used in the calculation, the default is the total CPU number you have.
#'
#' @return list containing the data frame of covariates for vegetated pixels and the corresponding index of the pixels.
#' 
#' @author Dongchen Zhang
#' @importFrom foreach %dopar%
stack_covariates_2_df <- function(rast.dir, cores = parallel::detectCores()) {
  # load maps.
  all.rast <- terra::rast(rast.dir)
  # parallel loop.
  layer.names <- names(all.rast)
  # register parallel nodes.
  if (cores > length(layer.names)) {
    cores <- length(layer.names)
  }
  cl <- parallel::makeCluster(as.numeric(cores))
  doSNOW::registerDoSNOW(cl)
  #progress bar.
  pb <- utils::txtProgressBar(min=1, max=length(layer.names), style=3)
  progress <- function(n) utils::setTxtProgressBar(pb, n)
  opts <- list(progress=progress)
  # foreach loop.
  r <- NULL
  vecs <- foreach::foreach(r = seq_along(layer.names), 
                           .packages=c("Kendall", "terra"),
                           .options.snow=opts) %dopar% {
                             all.rast <- terra::rast(rast.dir)
                             temp.vec <- matrix(all.rast[[r]], byrow = T)
                             na.inds <- which(is.na(temp.vec))
                             # if it's LC layer.
                             if ("LC" == names(all.rast)[r]) {
                               non.veg.inds <- which(! temp.vec %in% 1:8)
                               # non.veg.inds <- which(! temp.vec %in% 0:11)
                               na.inds <- unique(c(na.inds, non.veg.inds))
                             }
                             return(list(vec = temp.vec,
                                         na.inds = na.inds))
                           }
  # stop parallel.
  parallel::stopCluster(cl)
  foreach::registerDoSEQ()
  gc()
  # grab uniqued NA index.
  na.inds <- vecs %>% purrr::map("na.inds") %>% unlist %>% unique
  # remove NA from each covariate.
  cov.vecs <- vecs %>% purrr::map(function(v){
    return(v$vec[-na.inds])
  }) %>% dplyr::bind_cols() %>% `colnames<-`(layer.names) %>% as.data.frame()
  non.na.inds <- seq_along(matrix(all.rast[[1]]))[-na.inds]
  return(list(df = cov.vecs, non.na.inds = non.na.inds))
}

#' @description
#' This function helps to create the training dataset of specific variable type and locations for downscaling.
#' TODO: Add a ratio argument (training sample size/total sample size) so that we could calculate the out-of-sample accuracy.
#' @title prepare_train_dat
#' 
#' @param pts spatialpoints: spatial points returned by `terra::vectors` function.
#' @param analysis numeric: data frame (rows: ensemble member; columns: site*state_variables) of updated ensemble analysis results from the `sda_enkf` function.
#' @param covariates.dir character: path to the exported covariates GeoTIFF file.
#' @param variable character: name of state variable. It should match up with the column names of the analysis data frame. 
#'
#' @return matrix (num.sites, num.variables * num.ensemble + num.covariates) within which the first sets of columns contain values of state variables for each ensemble member of every site, and the rest columns contain the corresponding covariates.
#' 
#' @author Dongchen Zhang
prepare_train_dat <- function(pts, analysis, covariates.dir, variable) {
  # read covariates.
  cov.rast <- terra::rast(covariates.dir)
  # extract covariates by locations.
  predictors <- as.data.frame(terra::extract(cov.rast, pts, ID = FALSE))
  covariate_names <- names(predictors)
  if ("ID" %in% covariate_names) {
    rm.ind <- which("ID" %in% covariate_names)
    covariate_names <- covariate_names[-rm.ind]
    predictors <- predictors[,-rm.ind]
  }
  # grab carbon data.
  var.dat <- analysis[,which(colnames(analysis) == variable)] %>% t %>% 
    as.data.frame() %>% `colnames<-`(paste0("ensemble", seq(nrow(analysis))))
  # combine carbon and predictor.
  full_data <- cbind(var.dat, predictors)
  full_data <- full_data[which(full_data$LC %in% 1:8),]
  return(full_data)
}

#' @description
#' This function helps to train the ML model across ensemble members in parallel.
#' @title parallel_train
#' 
#' @param full_data numeric: the matrix generated using the `prepare_train_dat` function.
#' @param method character: machine learning method (currently support randomForest and xgboost).
#' @param cores numeric: how many CPus to be used in the calculation, the default is the total CPU number you have.
#'
#' @return list of trained models across ensemble members.
#' 
#' @author Dongchen Zhang
#' @importFrom foreach %dopar%
parallel_train <- function(full_data, method = "randomForest", cores = parallel::detectCores()) {
  # grab ensemble and predictor index.
  col.names <- colnames(full_data)
  ensemble.inds <- which(grepl("ensemble", col.names, fixed = TRUE))
  predictor.inds <- seq_along(col.names)[-ensemble.inds]
  # parallel train.
  # register parallel nodes.
  if (cores > length(ensemble.inds)) {
    cores <- length(ensemble.inds)
  }
  cl <- parallel::makeCluster(as.numeric(cores))
  doSNOW::registerDoSNOW(cl)
  #progress bar.
  pb <- utils::txtProgressBar(min=1, max=length(ensemble.inds), style=3)
  progress <- function(n) utils::setTxtProgressBar(pb, n)
  opts <- list(progress=progress)
  # foreach loop.
  i <- NULL
  models <- foreach::foreach(i = ensemble.inds, 
                             .packages=c("Kendall", "stats", method),
                             .options.snow=opts) %dopar% {
                               ensemble_col <- col.names[ensemble.inds[i]]
                               predictor_col <- col.names[predictor.inds]
                               # if it's randomForest.
                               if (method == "randomForest") {
                                 formula <- stats::as.formula(paste(ensemble_col, "~", paste(predictor_col, collapse = " + ")))
                                 model <- randomForest::randomForest(formula,
                                                                     data = full_data,
                                                                     ntree = 1000,
                                                                     na.action = stats::na.omit,
                                                                     keep.forest = TRUE,
                                                                     importance = TRUE)
                               }
                               # if it's xgboost.
                               if (method == "xgboost") {
                                 formula <- stats::as.formula(paste0("~ ", paste(predictor_col, collapse = " + "), " - 1"))
                                 train.df  <- stats::model.matrix(formula, data = full_data)
                                 train.df  <- xgboost::xgb.DMatrix(data = train.df, label = full_data[[ensemble_col]])
                                 model <- xgboost::xgb.train(
                                   params   = list(
                                     objective        = "reg:squarederror",
                                     eta              = 0.1,
                                     max_depth        = 6,
                                     subsample        = 0.8,
                                     colsample_bytree = 0.8
                                   ),
                                   data    = train.df,
                                   nrounds = 1000,
                                   nthread = 1,
                                   verbose = 0
                                 )
                               }
                               model
                             }
  # stop parallel.
  parallel::stopCluster(cl)
  foreach::registerDoSEQ()
  gc()
  return(models)
}

#' @description
#' This function helps to predict the target variable observations based on the covariates.
#' The prediction is working in parallel across vegetated pixels.
#' @title parallel_prediction
#' 
#' @param base.map.dir character: path to the GeoTIFF file within which the extents and CRS will be used to generate the ensemble maps.
#' @param models list: trained models across ensemble members generated by the `parallel_train` function.
#' @param cov.vecs numeric: data frame containing covaraites across vegetated pixels generated from the `stack_covariates_2_df` function.
#' @param non.na.inds numeric: the corresponding index of vegetated pixels generated from the `stack_covariates_2_df` function.
#' @param outdir character: the output directory where the downscaled maps will be stored.
#' @param name list: containing the time and variable name to create the final GeoTIFF file name.
#' @param cores numeric: how many CPus to be used in the calculation, the default is the total CPU number you have.
#'
#' @return paths to the ensemble downscaled maps.
#' 
#' @author Dongchen Zhang
#' @importFrom foreach %dopar%
parallel_prediction <- function(base.map.dir, models, cov.vecs, non.na.inds, outdir, name, cores = parallel::detectCores()) {
  # load base map.
  base.map <- terra::rast(base.map.dir)
  dims <- dim(base.map)
  # setup progress bar for ensemble members.
  pb <- utils::txtProgressBar(min = 0, max = length(models), style = 3)
  paths <- c()
  # loop over ensemble members.
  for (i in seq_along(models)) {
    # update progress bar.
    utils::setTxtProgressBar(pb, i)
    # go to the next if the current file has already been generated.
    file.name <- paste0(c("ensemble", i, name$time, name$variable), collapse = "_")
    if (file.exists(file.path(outdir, paste0(file.name, ".tiff")))) {
      next
    }
    # register parallel nodes.
    cl <- parallel::makeCluster(cores)
    doSNOW::registerDoSNOW(cl)
    # foreach parallel.
    model <- models[[i]]
    d <- NULL
    output <- foreach::foreach(d=itertools::isplitRows(cov.vecs, chunks=cores),
                               .packages=c("stats", "randomForest")) %dopar% {
                                 stats::predict(model, d)
                               } %>% unlist
    # export to geotiff map.
    vec <- rep(NA, dims[1]*dims[2])
    vec[non.na.inds] <- output
    map <- terra::rast(matrix(vec, dims[1], dims[2], byrow = T))
    terra::ext(map) <- terra::ext(base.map)
    terra::crs(map) <- terra::crs(base.map)
    terra::writeRaster(map, file.path(outdir, paste0(file.name, ".tiff")))
    paths <- c(paths, file.path(outdir, paste0(file.name, ".tiff")))
    # stop parallel.
    parallel::stopCluster(cl)
    foreach::registerDoSEQ()
    gc()
  }
  return(paths)
}

#' @description
#' This is the main function to execute the machine learning training and prediction. 
#' Note it will be deployed by each node you requested if the qsub feature is enabled below.
#' @title downscale_main
#' 
#' @param settings character: physical path that points to the pecan settings XML file.
#' @param analysis numeric: data frame (rows: ensemble member; columns: site*state_variables) of updated ensemble analysis results from the `sda_enkf` function.
#' @param covariates.dir character: path to the exported covariates GeoTIFF file.
#' @param time character: the time tag used to differentiate the outputs from others.
#' @param variable character: name of state variable. It should match up with the column names of the analysis data frame. 
#' @param outdir character: the output directory where the downscaled maps will be stored.
#' @param base.map.dir character: path to the GeoTIFF file within which the extents and CRS will be used to generate the ensemble maps.
#' @param method character: machine learning method, default is randomForest (currently support randomForest and xgboost).
#' @param cores numeric: how many CPus to be used in the calculation, the default is the total CPU number you have.
#'
#' @return paths to the ensemble downscaled maps.
#' 
#' @author Dongchen Zhang
downscale_main <- function(settings, analysis, covariates.dir, time, variable, outdir, base.map.dir, method = "randomForest", cores = parallel::detectCores()) {
  # check packages.
  if (method %in% rownames(utils::installed.packages())) {
    PEcAn.logger::logger.info(paste("The package:", method, "is not installed."))
    return(0)
  }
  if ("itertools" %in% rownames(utils::installed.packages())) {
    PEcAn.logger::logger.info("The package: itertools is not installed.")
    return(0)
  }
  if ("doSNOW" %in% rownames(utils::installed.packages())) {
    PEcAn.logger::logger.info("The package: doSNOW is not installed.")
    return(0)
  }
  if ("foreach" %in% rownames(utils::installed.packages())) {
    PEcAn.logger::logger.info("The package: foreach is not installed.")
    return(0)
  }
  # create folder specific for the time and carbon type.
  folder.name <- file.path(outdir, paste0(c(variable, time), collapse = "_"))
  if (!file.exists(folder.name)) {
    dir.create(folder.name)
  }
  # prepare training data.
  PEcAn.logger::logger.info("Preparing training data.")
  # convert settings into geospatial points.
  pts <- pecan_settings_2_pts(settings)
  full_data <- prepare_train_dat(pts = pts, 
                                 analysis = analysis, 
                                 covariates.dir = covariates.dir, 
                                 variable = variable)
  # convert LC into factor.
  if ("LC" %in% colnames(full_data)) {
    full_data[,"LC"] <- factor(full_data[,"LC"])
  }
  # parallel train.
  PEcAn.logger::logger.info("Parallel training.")
  models <- parallel_train(full_data = full_data, method = method, cores = cores)
  # save trained models for future analysis.
  # saveRDS(models, file.path(folder.name, "rf_models.rds"))
  save(models, file = file.path(folder.name, "ml_models.Rdata"))
  # convert stacked covariates geotiff file into data frame.
  PEcAn.logger::logger.info("Converting geotiff to df.")
  cov.df <- stack_covariates_2_df(rast.dir = covariates.dir, cores = cores)
  # reconstruct LC because of the computation accuracy.
  # cov.df$df$LC[which(cov.df$df$LC < 1)] <- 0
  # convert LC into factor.
  if ("LC" %in% colnames(cov.df$df)) {
    cov.df$df[,"LC"] <- factor(cov.df$df[,"LC"])
  }
  # parallel prediction.
  PEcAn.logger::logger.info("Parallel prediction.")
  paths <- parallel_prediction(base.map.dir = base.map.dir, 
                               models = models, 
                               cov.vecs = cov.df$df, 
                               non.na.inds = cov.df$non.na.inds, 
                               outdir = folder.name, 
                               name = list(time = as.character(time), variable = variable), 
                               cores = cores)
  # calculate mean and std.
  PEcAn.logger::logger.info("Calculate mean and std.")
  ras.all <- terra::rast(paths)
  mean <- terra::app(ras.all, "mean")
  std <- terra::app(ras.all, "std")
  # write into geotiff files.
  image.base.name <- paste0(time, "_", variable, ".tiff")
  terra::writeRaster(mean, filename = file.path(folder.name, paste0("mean_", image.base.name)))
  terra::writeRaster(std, filename = file.path(folder.name, paste0("std_", image.base.name)))
  return(list(ensemble.prediction.files = paths,
              mean.prediction.file = file.path(folder.name, paste0("mean_", image.base.name)),
              std.prediction.file = file.path(folder.name, paste0("std_", image.base.name))))
}

#' @description
#' This qsub function helps to run the submitted qsub jobs for running the downscale_main function.
#' @title downscale_qsub_main
#' 
#' @param folder.path Character: physical path to which the job file is located.
#' 
#' @export
#' @author Dongchen Zhang
downscale_qsub_main <- function(folder.path) {
  dat <- readRDS(file.path(folder.path, "dat.rds"))
  out <- downscale_main(dat$settings, dat$analysis.yr, dat$covariates.dir, lubridate::year(dat$time), dat$variable, dat$outdir, dat$base.map.dir, dat$method, dat$cores)
  saveRDS(out, file.path(folder.path, "res.rds"))
}