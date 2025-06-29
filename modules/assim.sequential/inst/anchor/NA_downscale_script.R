library(purrr)
library(foreach)
library(PEcAnAssimSequential)
setwd("/projectnb/dietzelab/dongchen/anchorSites/NA_runs/")
# prepare stand age time-series.
modis.lc.folder <- "/projectnb/dietzelab/dongchen/anchorSites/NA_runs/MODIS_LC/LC"
stand.age.out.folder <- "/projectnb/dietzelab/dongchen/anchorSites/NA_runs/MODIS_LC/stand_age"
# filter land cover time-series.
# loop over years.
# read tiff file.
forest_type <- c(1:4)
grass_type <- c(5:8)
non_veg_type <- c(0, 9, 10, 11)
base.map <- terra::rast("/projectnb/dietzelab/dongchen/anchorSites/downscale/base_map.tiff")
base_crs <- terra::crs(base.map)
base_ext <- terra::ext(base.map)
# load forest age data.
forest_age <- matrix(terra::rast("/projectnb/dietzelab/dongchen/anchorSites/downscale/forest_age/forest_age_2010_TC000_crop.tiff"), byrow = T)
# calculate mean age for different LC types.
LC <- matrix(terra::rast(file.path(modis.lc.folder, paste0(2010, ".tif")))[[5]], byrow = T)
mean_age <- c()
for (i in 1:8) {
  mean_age <- c(mean_age, mean(forest_age[which(LC == i)], na.rm = T))
}
# function for filtering time series.
filter.lc.ts <- function(vec, window.L = 3) {
  L <- length(vec)
  window <- c()
  edge.case <- FALSE
  # 
  if(length(unique(vec)) == 1) {
    return(c(unique(vec), unique(vec), length(vec)))
  }
  for (i in L:1) {
    # push item into the window.
    window <- c(window, vec[i])
    # print(window)
    # if window has not reached its size.
    if (length(window) < window.L) {
      next
    }
    # window operation.
    uni.val <- unique(window)
    # if there is no change.
    if (length(uni.val) == 1) {
      
    } else if (length(uni.val) > 1) {
      # check if head == tail.
      if (head(window, 1) == tail(window, 1)) {
        
      } else {
        window.ind <- window.L - tail(which(window == head(uni.val, 1)), 1) + 1
        return(c(tail(rev(uni.val), 2), L - i + 1 - window.ind + 1))
      }
    }
    # remove the last item from the window.
    window <- tail(window, -1)
  }
  # if there is no disturbance afterall.
  return(c(vec[length(vec)], vec[length(vec)], length(vec)))
}

# store MODIS land cover time-series into matrix.
ts_lc <- c()
for (end.year in 2012:2023) {
  print(end.year)
  if (end.year == 2012) {
    start.year <- 2001
  } else {
    start.year <- end.year
  }
  # load last year MODIS LC map.
  LC <- matrix(terra::rast(file.path(modis.lc.folder, paste0(end.year, ".tif")))[[5]], byrow = T)
  # store MODIS land cover time-series into matrix.
  # ts_lc <- c()
  for (y in start.year:end.year) {
    # load image.
    lc_tif <- terra::rast(file.path(modis.lc.folder, paste0(y, ".tif")))
    lc_matrix <- matrix(lc_tif[[5]], byrow = T)
    # reclassify.
    lc_matrix[which(lc_matrix %in% forest_type)] <- 1
    lc_matrix[which(lc_matrix %in% grass_type)] <- 2
    lc_matrix[which(lc_matrix %in% non_veg_type)] <- 3
    # combine image.
    ts_lc <- cbind(ts_lc, lc_matrix)
    # print(y)
  }
  # loop over NA.
  split_data.matrix <- function(matrix, chunk.size=100) {
    ncols <- dim(matrix)[2]
    nchunks <- (ncols-1) %/% chunk.size + 1
    split.data <- list()
    min <- 1
    for (i in seq_len(nchunks)) {
      if (i == nchunks-1) {  #make last two chunks of equal size
        left <- ncols-(i-1)*chunk.size
        max <- min+round(left/2)-1
      } else {
        max <- min(i*chunk.size, ncols)
      }
      split.data[[i]] <- t(matrix[,min:max,drop=FALSE])
      min <- max+1    #for next chunk
    }
    return(split.data)
  }
  mat.lists <- split_data.matrix(t(ts_lc), floor(dim(ts_lc)[1]/parallel::detectCores()))
  # register parallel nodes.
  cl <- parallel::makeCluster(parallel::detectCores())
  doSNOW::registerDoSNOW(cl)
  res <- foreach::foreach(d = mat.lists, .packages=c("purrr")) %dopar% {
    temp.res <- matrix(NA, dim(d)[1], 4) %>% `colnames<-`(c("from", "to", "years", "type"))
    pb <- utils::txtProgressBar(min=1, max=dim(d)[1], style=3)
    for (i in 1:dim(d)[1]) {
      if (any(is.na(d[i,]))) next
      temp.res[i, 1:3] <- filter.lc.ts(d[i,])
      # grab change patterns.
      if (all(temp.res[i, 1:2] == c(1, 2))) {
        temp.res[i, 4] <- 1
      } else if (all(temp.res[i, 1:2] == c(1, 3))) {
        temp.res[i, 4] <- 2
      } else if (all(temp.res[i, 1:2] == c(2, 3))) {
        temp.res[i, 4] <- 3
      } else if (all(temp.res[i, 1:2] == c(2, 1))) {
        temp.res[i, 4] <- 4
      } else if (all(temp.res[i, 1:2] == c(3, 1))) {
        temp.res[i, 4] <- 5
      } else if (all(temp.res[i, 1:2] == c(3, 2))) {
        temp.res[i, 4] <- 6
      }
      utils::setTxtProgressBar(pb, i)
    }
    return(temp.res)
  }
  res <- do.call(rbind, res)
  # any pixel in forest that are tagged as grassland should be replaced with the 
  # load forest age data.
  forest_age <- matrix(terra::rast("/projectnb/dietzelab/dongchen/anchorSites/downscale/forest_age/forest_age_2010_TC000_crop.tiff"), byrow = T)
  forest_age <- cbind(forest_age, res, LC) %>% `colnames<-`(c("forest_age", "from", "to", "years", "type", "LC"))
  forest_age <- split_data.matrix(t(forest_age), floor(dim(forest_age)[1]/parallel::detectCores()))
  forest_age  <- foreach::foreach(d = forest_age, .packages=c("purrr")) %dopar% {
    for (i in 1:dim(d)[1]) {
      # if it's diturbed vegetation.
      if (is.na(d[i, "years"])) next
      if (d[i, "years"] < (end.year - 2000)) {
        d[i, "forest_age"] <- d[i, "years"]
        next
      }
      # no record for the forest age.
      if (is.na(d[i, "forest_age"])) {
        # if it is non vegetation.
        if (d[i, "to"] == 3) {
          # forest_age[i] <- 0
          next
        } else {
          # if it's non-disturbed vegetation.
          d[i, "forest_age"] <- mean_age[d[i, "LC"]]
        }
      }
    }
    return(d)
  }
  forest_age <- do.call(rbind, forest_age)
  # stop parallel.
  parallel::stopCluster(cl)
  foreach::registerDoSEQ()
  # write to raster.
  forest_age <- terra::rast(matrix(forest_age[,"forest_age"], 9360, 19080, byrow = T))
  terra::ext(forest_age) <- base_ext
  terra::crs(forest_age) <- base_crs
  names(forest_age) <- "year_since_disturb"
  terra::writeRaster(forest_age, file=file.path(stand.age.out.folder, paste0(end.year, "_stand_age.tif")))
  gc()
}
# average ERA5 to climatic covariates.
outdir <- "/projectnb/dietzelab/dongchen/anchorSites/NA_runs/GridMET"
in.path <- "/projectnb/dietzelab/dongchen/anchorSites/ERA5/"
start.dates <- c("2012-01-01", "2012-07-16", "2013-07-16", 
                 "2014-07-16", "2015-07-16", "2016-07-16", 
                 "2017-07-16", "2018-07-16", "2019-07-16", 
                 "2020-07-16", "2021-07-16", "2022-07-16", 
                 "2023-07-16")
end.dates <- c("2012-07-15", "2013-07-15", "2014-07-15", 
               "2015-07-15", "2016-07-15", "2017-07-15", 
               "2018-07-15", "2019-07-15", "2020-07-15", 
               "2021-07-15", "2022-07-15", "2023-07-15", 
               "2024-07-15")
# parallel average ERA5 into covariates.
future::plan(future::multisession, workers = 5, gc = T)
paths <- start.dates %>% furrr::future_map2(end.dates, function(d1, d2){
  PEcAn.data.atmosphere::Average_ERA5_2_GeoTIFF(d1, d2, in.path, outdir)
}, .progress = T) %>% unlist
# create covariates time series.
for (y in 2012:2024) {
  print(y)
  if (y == 2024) {
    y.lc <- 2023
  } else {
    y.lc <- y
  }
  # LC <- file.path("/projectnb/dietzelab/dongchen/anchorSites/NA_runs/MODIS_LC/LC", paste0(y.lc, ".tif"))
  LC <- "/projectnb/dietzelab/dongchen/anchorSites/downscale/MODIS_NLCD_LC.tif"
  stand.age <- file.path("/projectnb/dietzelab/dongchen/anchorSites/NA_runs/MODIS_LC/stand_age", paste0(y.lc, "_stand_age.tif"))
  met <- list.files("/projectnb/dietzelab/dongchen/anchorSites/NA_runs/GridMET", full.names = T)
  met <- met[which(grepl(y, met))]
  # setup covariates paths and variable names.
  cov.tif.file.list <- list(LC = list(dir = LC,
                                      var.name = "LC"),
                            year_since_disturb = list(dir = stand.age,
                                                      var.name = "year_since_disturb"),
                            agb = list(dir = "/projectnb/dietzelab/dongchen/anchorSites/downscale/AGB/agb.tif",
                                       var.name = "agb"),
                            twi = list(dir = "/projectnb/dietzelab/dongchen/anchorSites/downscale/TWI/TWI_resample.tiff",
                                       var.name = "twi"),
                            met = list(dir = met,
                                       var.name = c("temp", "prec", "srad", "vapr")),
                            soil = list(dir = "/projectnb/dietzelab/dongchen/anchorSites/downscale/SoilGrids.tif",
                                        var.name = c("PH", "N", "SOC", "Sand")))
  covariates.dir <- stack_covariates_2_geotiff(outdir = "/projectnb/dietzelab/dongchen/anchorSites/NA_runs/SDA_50ens_2025_4_4/covariates", 
                                               year = y,
                                               base.map.dir = "/projectnb/dietzelab/dongchen/anchorSites/downscale/base_map.tiff", 
                                               cov.tif.file.list = cov.tif.file.list, 
                                               normalize = T, 
                                               cores = parallel::detectCores())
}

# setup parallel downscaling.
method <- "randomForest"
base.map.dir <- "/projectnb/dietzelab/dongchen/anchorSites/downscale/MODIS_NLCD_LC.tif"
load("/projectnb/dietzelab/dongchen/anchorSites/NA_runs/SDA/sda.all.forecast.analysis.Rdata")
variables <- c("AbvGrndWood", "LAI", "SoilMoistFrac", "TotSoilCarb")
settings <- "/projectnb/dietzelab/dongchen/anchorSites/NA_runs/AGU_2024/pts.shp"
outdir <- "/projectnb/dietzelab/dongchen/anchorSites/NA_runs/SDA/"
cores <- 28
date <- seq(as.Date("2012-07-15"), as.Date("2024-07-15"), "1 year")
# loop over years.
for (i in seq_along(date)) {
  print(i)
  # Assemble covariates.
  covariates.dir <- file.path("/projectnb/dietzelab/dongchen/anchorSites/NA_runs/covariates_lc_ts/", paste0("covariates_", lubridate::year(date[i]), ".tiff"))
  # grab analysis.
  analysis.yr <- analysis.all[[i]]
  time <- date[i]
  # loop over carbon types.
  for (j in seq_along(variables)) {
    # setup folder.
    variable <- variables[j]
    folder.path <- file.path(file.path(outdir, "downscale_maps_analysis_lc_ts"), paste0(variables[j], "_", date[i]))
    dir.create(folder.path)
    saveRDS(list(settings = settings, 
                 analysis.yr = analysis.yr, 
                 covariates.dir = covariates.dir, 
                 time = time, 
                 variable = variable, 
                 folder.path = folder.path, 
                 base.map.dir = base.map.dir,
                 method = method,
                 cores = cores, 
                 outdir = file.path(outdir, "downscale_maps_analysis_lc_ts")),
         file = file.path(folder.path, "dat.rds"))
    # prepare for qsub.
    jobsh <- c("#!/bin/bash -l", 
               "module load R/4.1.2", 
               "echo \"require (PEcAnAssimSequential)", 
               "      require (foreach)",
               "      require (purrr)",
               "      downscale_qsub_main('@FOLDER_PATH@')", 
               "    \" | R --no-save")
    jobsh <- gsub("@FOLDER_PATH@", folder.path, jobsh)
    writeLines(jobsh, con = file.path(folder.path, "job.sh"))
    # qsub command.
    qsub <- "qsub -l h_rt=24:00:00 -l buyin -pe omp @CORES@ -V -N @NAME@ -o @STDOUT@ -e @STDERR@ -S /bin/bash"
    qsub <- gsub("@CORES@", cores, qsub)
    qsub <- gsub("@NAME@", paste0("ds_", i, "_", j), qsub)
    qsub <- gsub("@STDOUT@", file.path(folder.path, "stdout.log"), qsub)
    qsub <- gsub("@STDERR@", file.path(folder.path, "stderr.log"), qsub)
    qsub <- strsplit(qsub, " (?=([^\"']*\"[^\"']*\")*[^\"']*$)", perl = TRUE)
    cmd <- qsub[[1]]
    out <- system2(cmd, file.path(folder.path, "job.sh"), stdout = TRUE, stderr = TRUE)
  }
}
