#' Prepare L4A GEDI above ground biomass (AGB) data for the state data assimilation (SDA) workflow.
#' @details During the first use, users will need to create the 
#'  `.nasadaacapirc` file in the out folder where the first and second lines 
#'  are the username and password on the NASA Earth Explore server. 
#'  If you don't have an account, register at https://urs.earthdata.nasa.gov/users/new.
#'
#' @param site_info List: A list including site_id, longitude, and latitude.
#' @param time_points Character: A vector of date contains target dates (in YYYY-MM-DD).
#' @param outdir Character: Directory where the final CSV file will be stored.
#' @param buffer Numeric: buffer distance (in degrees) for locate GEDI AGB searching box (default is 0.005 [~ 500 m]).
#' @param search_window Character: search window (any length of time. e.g., 6 month) for locate available GEDI AGB values.
#' @param bbox Numeric: the vector (in xmin, xmax, ymin, and ymax) that covers all the sites in the site_info object (default is NULL).
#' @param batch Boolean: determine if we want to submit jobs to the queue or not (default is FALSE).
#' @param prerun Character: series of pre-launch shell command before running the shell job (default is NULL).
#' @param num.folder Numeric: the number of batch folders to be created when submitting jobs to the queue.
#' @param cores Numeric: numbers of core to be used for the parallel computation. The default is the maximum current CPU number.
#' @param credential.folder Character: the physical path to the folder that contains the credential file (.nasadaacapirc).
#'
#' @return A data frame containing AGB and sd for each site and each time step.
#' @export
#' 
#' @examples
#' \dontrun{
#' settings <- PEcAn.settings::read.settings("pecan.xml")
#' site_info <- settings %>% 
#'   purrr::map(~.x[['run']] ) %>% 
#'   purrr::map('site')%>% 
#'   purrr::map(function(site.list){
#'     #conversion from string to number
#'     site.list$lat <- as.numeric(site.list$lat)
#'     site.list$lon <- as.numeric(site.list$lon)
#'     list(site_id=site.list$id, lat=site.list$lat, lon=site.list$lon, site_name=site.list$name)
#'   }) %>% 
#'   dplyr::bind_rows() %>% 
#'   as.list()
#' time_points <- seq(start.date, end.date, by = time.step)
#' buffer <- 0.005
#' outdir <- getwd()
#' GEDI_AGB <- GEDI_AGB_prep(site_info, time_points, outdir, buffer)
#' }
#' @author Dongchen Zhang
#' @importFrom purrr %>%
GEDI_AGB_prep <- function(site_info, 
                          time_points, 
                          outdir = file.path(getwd(), "GEDI_AGB"), 
                          buffer = 0.005, 
                          search_window = "6 month", 
                          bbox = NULL,
                          batch = FALSE, 
                          prerun = NULL,
                          num.folder = NULL,
                          cores = parallel::detectCores(),
                          credential.folder = "~") {
  # convert list to vector.
  if (is.list(bbox)) {
    bbox <- as.numeric(unlist(bbox))
  }
  if (is.list(prerun)) {
    prerun <- unlist(prerun)
  }
  # calculate the bbox if it's not inputted.
  if (is.null(bbox)) {
    bbox <- c(min(site_info$lon) - buffer,
              max(site_info$lon) + buffer,
              min(site_info$lat) - buffer,
              max(site_info$lat) + buffer)
  }
  # if we don't have outdir, we will use the temp dir as outdir.
  if (!dir.exists(outdir)) {
    dir.create(outdir)
  }
  # detect if we generate the NASA DAAC credential file.
  if (!file.exists(file.path(credential.folder, ".nasadaacapirc"))) {
    PEcAn.logger::logger.info("There is no credential file for NASA DAAC server.")
    PEcAn.logger::logger.info("Please create the .nasadaacapirc file within the credential folder.")
    PEcAn.logger::logger.info("The first and second lines of the file are the username and password.")
    return(NULL)
  }
  # check dates.
  time_points <- time_points[which(time_points >= as.Date("2019-04-18"))]
  # if we don't have any observation for those dates.
  if (length(time_points) == 0) {
    return(NULL)
  }
  # if we have dates with observations.
  # summarize data lists into data frame.
  AGB_Output <- matrix(NA, length(site_info$site_id), 2*length(time_points)+1) %>% 
    `colnames<-`(c("site_id", paste0(time_points, "_AGB"), paste0(time_points, "_SD"))) %>% as.data.frame()#we need: site_id, AGB, std, target time point.
  AGB_Output$site_id <- site_info$site_id
  # loop over each time point.
  for (i in seq_along(time_points)) {
    # create start and end dates.
    start_date <- seq(time_points[i], length.out = 2, by = paste0("-", search_window))[2]
    end_date <- seq(time_points[i], length.out = 2, by = search_window)[2]
    # create the download folder for downloaded GEDI tiles.
    download.path <- file.path(outdir, "download")
    if (!dir.exists(download.path)) {
      dir.create(download.path)
    } else {
      # delete previous downloaded files.
      unlink(download.path, recursive = T)
      dir.create(download.path)
    }
    # download GEDI tiles.
    files <- NASA_DAAC_download(ul_lat = bbox[4], 
                                ul_lon = bbox[1], 
                                lr_lat = bbox[3], 
                                lr_lon = bbox[2], 
                                ncore = cores, 
                                from = start_date, 
                                to = end_date, 
                                outdir = download.path, 
                                doi = "10.3334/ORNLDAAC/2056", 
                                just_path = F, 
                                credential.folder = credential.folder)
    # if we want to submit jobs to the queue.
    if (batch) {
      if (is.null(num.folder)) {
        PEcAn.logger::logger.info("Please provide the number of batch folders if you want to submit jobs to the queue!")
        return(NULL)
      }
      which.point.in.which.file <- GEDI_L4A_Finder_batch(files = files, 
                                                         outdir = outdir, 
                                                         site_info = site_info, 
                                                         num.folder = as.numeric(num.folder), 
                                                         buffer = as.numeric(buffer), 
                                                         cores = as.numeric(cores), 
                                                         prerun = prerun)
      agb <- GEDI_L4A_2_mean_var.batch(site_info = site_info, 
                                       outdir = outdir, 
                                       which.point.in.which.file = which.point.in.which.file, 
                                       num.folder = as.numeric(num.folder), 
                                       buffer = as.numeric(buffer), 
                                       cores = as.numeric(cores), 
                                       prerun = prerun)
    } else {
      # if we want to run the job locally.
      which.point.in.which.file <- GEDI_L4A_Finder_batch(files = files, 
                                                         site_info = site_info, 
                                                         buffer = as.numeric(buffer), 
                                                         cores = as.numeric(cores))
      agb <- GEDI_L4A_2_mean_var.batch(site_info = site_info, 
                                       which.point.in.which.file = which.point.in.which.file, 
                                       buffer = as.numeric(buffer), 
                                       cores = as.numeric(cores))
    }
    # delete previous downloaded files.
    unlink(download.path, recursive = T)
    # loop over sites.
    for (j in seq_len(nrow(agb))) {
      # skip NA observations.
      if (is.na(agb$agb_mean[j])) {
        next
      }
      # otherwise calculate the mean and standard error.
      AGB_Output[j, paste0(time_points[i], "_AGB")] <- agb$agb_mean[j] # mean
      AGB_Output[j, paste0(time_points[i], "_SD")] <- agb$agb_sd[j] # sd
    }
  }
  return(list(AGB_Output = AGB_Output, time_points = time_points, var = "AGB"))
}
#' Detect which GEDI level 4A tiles intercept which site.
#'
#' @param files Character: full paths of GEDI level 4A tiles.
#' @param site_info List: list of site info including site_id, site_name, lon, and lat.
#' @param buffer Numeric: buffer distance (in degree) that is used to create the bounding box (default is 0.005 [~ 500 m]).
#' @param cores Numeric: numbers of core to be used for the parallel computation. The default is the maximum current CPU number.
#' 
#' @return A list containing physical paths of GEDI tiles that intercept the each site.
#' @export
GEDI_L4A_Finder <- function(files, 
                            site_info, 
                            buffer = 0.005, 
                            cores = parallel::detectCores()) {
  # report current workflow.
  PEcAn.logger::logger.info("Intersecting GEDI tiles with sites.")
  # grab coordinates from site_info object.
  lats <- site_info$lat
  lons <- site_info$lon
  # initialize parallel.
  cl <- parallel::makeCluster(as.numeric(cores))
  doSNOW::registerDoSNOW(cl)
  # setup progress bar.
  pb <- utils::txtProgressBar(min=1, max=length(files), style=3)
  which.point.in.which.file <- vector("list", length = length(lats))
  # loop over files.
  for (i in seq_along(files)) {
    # load file.
    level4a_h5 <- hdf5r::H5File$new(files[i], mode = "r")
    # load beams id.
    groups_id <- grep("BEAM\\d{4}$", gsub("/", "", hdf5r::list.groups(level4a_h5,recursive = F)), value = T)
    # loop over beams.
    dat <- c()
    for (j in groups_id) {
      level4a_i <- level4a_h5[[j]]
      if (any(hdf5r::list.datasets(level4a_i) == "shot_number")) {
        rhs <- data.table::data.table(lat_lowestmode = level4a_i[["lat_lowestmode"]][],
                                      lon_lowestmode = level4a_i[["lon_lowestmode"]][],
                                      agbd = level4a_i[["agbd"]][])
        
        rhs <- rhs[which(rhs$agbd>=0),]
        dat <- rbind(dat, rhs)
      }
    }
    if (nrow(dat) == 0) next
    # determine which sites are within the current GEDI tile.
    # resolve GitHub namespace checking.
    lat <- lon <- NULL
    res <- foreach::foreach(lat = lats, lon = lons) %dopar% {
      if (lat > 54 | lat < -54) {
        return(0)
      }
      diff.lat <- abs(lat - dat$lat_lowestmode)
      diff.lon <- abs(lon - dat$lon_lowestmode)
      keep.inds <- which(diff.lat <= buffer & diff.lon <= buffer)
      if (length(keep.inds) > 0) {
        return(1)
      } else {
        return(0)
      }
    } %>% unlist
    # load the file path to the corresponding site if intercepted.
    for (j in which(res == 1)) {
      which.point.in.which.file[[j]] <- c(which.point.in.which.file[[j]], files[i])
    }
    # update progress bar.
    utils::setTxtProgressBar(pb, i)
  }
  # stop parallel.
  parallel::stopCluster(cl)
  foreach::registerDoSEQ()
  return(which.point.in.which.file)
}
#' Submit jobs through `qsub` for the `GEDI_L4A_Finder` function.
#' 
#' @param files Character: full paths of GEDI level 4A tiles.
#' @param outdir Character: the physical path within which the batch job folders will be created.
#' @param site_info List: list of site info including site_id, site_name, lon, and lat.
#' @param num.folder Numeric: the number of batch folders to be created.
#' @param buffer Numeric: buffer distance (in degree) that is used to create the bounding box (default is 0.005 [~ 500 m]).
#' @param cores Numeric: numbers of core to be used for the parallel computation. The default is the maximum current CPU number.
#' @param prerun Character: a vector of strings that will be executed beforehand. The default is NULL.
#' 
#' @return A list containing physical paths of GEDI tiles that intercept the each site.
#' @export
GEDI_L4A_Finder_batch <- function(files, 
                                  outdir, 
                                  site_info, 
                                  num.folder, 
                                  buffer = 0.005, 
                                  cores = parallel::detectCores(), 
                                  prerun = NULL) {
  # report current workflow.
  PEcAn.logger::logger.info("Intersecting GEDI tiles with sites.")
  # how many files do we have.
  L <- length(files)
  # how many folders should be created.
  num.per.folder <- ceiling(L/num.folder)
  # create folder for storing job outputs.
  batch.folder <- file.path(outdir, "batch")
  # delete the whole folder if it's not empty.
  if (file.exists(batch.folder)){
    unlink(batch.folder, recursive = T)
  } 
  dir.create(batch.folder)
  folder.paths <- c()
  for (i in 1:num.folder) {
    # create folder for each set of pixels.
    head.num <- (i-1)*num.per.folder + 1
    # if the site number can not be evenly divided.
    if (i*num.per.folder > L) {
      tail.num <- L
    } else {
      tail.num <- i*num.per.folder
    }
    # create folder name based on start and end numbers.
    folder.name <- paste0("From_", head.num, "_to_", tail.num)
    folder.path <- file.path(batch.folder, folder.name)
    folder.paths <- c(folder.paths, folder.path)
    if (dir.exists(folder.path)) {
      unlink(x = file.path(folder.path, c("stderr.log", "stdout.log")))
    } else {
      dir.create(folder.path)
      # write parameters.
      configs <- list(folder.files = files[head.num:tail.num],
                      site_info = site_info,
                      buffer = buffer,
                      cores = cores)
      saveRDS(configs, file = file.path(folder.path, "configs.rds"))
    }
    jobsh <- c(prerun,  
               "echo \"require (purrr)",
               "       require (PEcAn.data.remote)",
               "       require (foreach)",
               "       configs <- readRDS(file.path('@FOLDER_PATH@', 'configs.rds'))",
               "       which.point.in.which.file <- GEDI_L4A_Finder(configs[[1]], configs[[2]], configs[[3]], configs[[4]])",
               "       saveRDS(which.point.in.which.file, file = file.path('@FOLDER_PATH@', 'res.rds'))",
               "    \" | R --no-save")
    jobsh <- gsub("@FOLDER_PATH@", folder.path, jobsh)
    writeLines(jobsh, con = file.path(folder.path, "job.sh"))
    # qsub command.
    qsub <- "qsub -l h_rt=1:00:00 -l buyin -pe omp @CORES@ -V -N @NAME@ -o @STDOUT@ -e @STDERR@ -S /bin/bash"
    qsub <- gsub("@CORES@", cores, qsub)
    qsub <- gsub("@NAME@", paste0("Job-", i), qsub)
    qsub <- gsub("@STDOUT@", file.path(folder.path, "stdout.log"), qsub)
    qsub <- gsub("@STDERR@", file.path(folder.path, "stderr.log"), qsub)
    qsub <- strsplit(qsub, " (?=([^\"']*\"[^\"']*\")*[^\"']*$)", perl = TRUE)
    cmd <- qsub[[1]]
    out <- system2(cmd, file.path(folder.path, "job.sh"), stdout = TRUE, stderr = TRUE)
    # if we reach the maximum file number.
    if (tail.num == L) {
      num.folder <- i
      break
    }
  }
  PEcAn.logger::logger.info("Checking outputs.")
  l <- length(list.files(batch.folder, pattern = "res.rds", recursive = T))
  pb <- utils::txtProgressBar(min = 0, max = num.folder, style = 3)
  while(l < num.folder) {
    Sys.sleep(10)
    l <- length(list.files(batch.folder, pattern = "res.rds", recursive = T))
    utils::setTxtProgressBar(pb, l)
  }
  # assemble results.
  PEcAn.logger::logger.info("Assembling results.")
  res.paths <- list.files(batch.folder, pattern = "res.rds", recursive = T, full.names = T)
  res <- vector("list", length = length(site_info$site_id))
  for (path in res.paths) {
    which.point.in.which.file <- readRDS(path)
    for (i in seq_along(res)) {
      if (is.null(which.point.in.which.file[[i]])) next
      res[[i]] <- c(res[[i]], which.point.in.which.file[[i]])
    }
  }
  return(res)
}
#' Aggregate AGB mean and uncertainty from the GEDI level4A tiles.
#'
#' @param site_info List: list of site info including site_id, site_name, lon, and lat.
#' @param which.point.in.which.file List: lists containing physical paths of GEDI tiles that intercept the each site.
#' @param buffer Numeric: buffer distance (in degree) that is used to create the bounding box (default is 0.005 [~ 500 m]).
#' @param cores Numeric: numbers of core to be used for the parallel computation. The default is the maximum current CPU number.
#' 
#' @return A list containing AGB mean and standard deviation for each site.
#' @export
GEDI_L4A_2_mean_var <- function(site_info, 
                                which.point.in.which.file, 
                                buffer = 0.005, 
                                cores = parallel::detectCores()) {
  # checking packages.
  if ("try-error" %in% class(try(find.package("hdf5r")))) {
    PEcAn.logger::logger.info("The hdf5r is not installed.")
    return(NA)
  }
  if ("try-error" %in% class(try(find.package("doSNOW")))) {
    PEcAn.logger::logger.info("The doSNOW is not installed.")
    return(NA)
  }
  # report current workflow.
  PEcAn.logger::logger.info("Estimating AGB mean and uncertainty.")
  # initialize agb mean and sd lists for each site.
  agb_mean <- agb_sd <- rep(NA, length(which.point.in.which.file))
  # initialize parallel.
  cl <- parallel::makeCluster(cores)
  doSNOW::registerDoSNOW(cl)
  # setup progress bar.
  pb <- utils::txtProgressBar(min=1, max=length(which.point.in.which.file), style=3)
  progress <- function(n) utils::setTxtProgressBar(pb, n)
  opts <- list(progress=progress)
  # resolve GitHub namespace checking.
  i <- NULL
  agb_mean_sd <- foreach::foreach(i = seq_along(which.point.in.which.file), .packages = c("Kendall", "purrr"), .options.snow=opts) %dopar% {
    temp.files <- which.point.in.which.file[[i]]
    # if we only have 1 or 0 files that cover this pixel.
    if (length(temp.files) <= 1) {
      return(list(agb_mean = NA, agb_sd = NA))
    }
    file.keep.dat <- vector("list", length = length(temp.files))
    # loop over files.
    for (j in seq_along(temp.files)) {
      # load file.
      level4a_h5 <- hdf5r::H5File$new(temp.files[j], mode = "r")
      # load beams id.
      groups_id <- grep("BEAM\\d{4}$", gsub("/", "", hdf5r::list.groups(level4a_h5,recursive = F)), value = T)
      dat <- c()
      for (beam in groups_id) {
        level4a_i <- level4a_h5[[beam]]
        # if we have that datasets for the specific beam.
        if (any(hdf5r::list.datasets(level4a_i) == "shot_number")) {
          # grab lat/lon, agb, and the orbit information.
          rhs <- data.table::data.table(lat_lowestmode = level4a_i[["lat_lowestmode"]][],
                                        lon_lowestmode = level4a_i[["lon_lowestmode"]][],
                                        agbd = level4a_i[["agbd"]][],
                                        orbit = substr(level4a_i[["shot_number"]][], 1, 5))
          xvar <- t(level4a_i[["xvar"]][,]) %>% `colnames<-`(c("RH25", "RH50", "RH75", "RH98"))
          predict_stratum <- level4a_i[["predict_stratum"]][]
          rhs <- cbind(rhs, xvar, predict_stratum)
          # grab records that have usable agb estimations.
          rhs <- rhs[which(rhs$agbd>=0),]
          dat <- rbind(dat, rhs)
        }
      }
      # filtering data by the bounding box.
      diff.lat <- abs(site_info$lat[i] - dat$lat_lowestmode)
      diff.lon <- abs(site_info$lon[i] - dat$lon_lowestmode)
      keep.inds <- which(diff.lat <= buffer & diff.lon <= buffer)
      file.keep.dat[[j]] <- list(dat = dat[keep.inds, ])
    }
    # grab ancillary table.
    ancillary <- level4a_h5[["ANCILLARY"]][["model_data"]]
    vcov <- array(unlist(lapply(1:35,function(x)ancillary[x][["vcov"]])), dim = c(5, 5, 35))
    predict_stratum <- do.call(rbind,lapply(1:35,function(x)ancillary[x][["predict_stratum"]]))
    # combining data and extract records that have the most abundant predict stratum.
    dat <- file.keep.dat %>% purrr::map(function(d){d$dat}) %>% dplyr::bind_rows()
    most_stratum <- names(sort(table(dat$predict_stratum), decreasing = TRUE))[1]
    for (j in seq_along(file.keep.dat)) {
      file.keep.dat[[j]]$dat <- file.keep.dat[[j]]$dat[which(file.keep.dat[[j]]$dat$predict_stratum == most_stratum),]
    }
    dat <- file.keep.dat %>% purrr::map(function(d){d$dat}) %>% dplyr::bind_rows()
    # loop over clusters by the unique orbit IDs.
    cluster.ids <- unique(dat$orbit)
    m <- length(cluster.ids)
    # if we only have one unique orbit.
    if (m == 1) {
      return(list(agb_mean = NA, agb_sd = NA))
    }
    # calculate number of footprints and sum agb per cluster.
    num.footprints <- agb.sum <- c()
    predictor <- rep(0, 4)
    for (id in cluster.ids) {
      num.footprints <- c(num.footprints, length(which(dat$orbit == id)))
      agb.sum <- c(agb.sum, sum(dat$agbd[which(dat$orbit == id)]))
      predictor <- predictor + colSums(dat[which(dat$orbit == id), 5:8])
    }
    # calculate the agb mean and uncertainty based on equation 5 in:
    # https://iopscience.iop.org/article/10.1088/1748-9326/ab18df/pdf.
    which.stratum <- which(predict_stratum == most_stratum)
    agb_est <- mean(agb.sum)/mean(num.footprints)
    sample.error <- sum((agb.sum - agb_est * num.footprints)^2)/(m*(m-1)*mean(num.footprints)^2)
    vcov <- vcov[,,which.stratum]
    mean.predictor <- c(mean(num.footprints), predictor/m)
    sd_est <- sqrt(t(mean.predictor) %*% vcov %*% mean.predictor + sample.error)
    return(list(agb_mean = agb_est, agb_sd = sd_est))
  }
  # stop parallel.
  parallel::stopCluster(cl)
  foreach::registerDoSEQ()
  agb_mean_sd <- data.frame(site_id = site_info$site_id, 
                            agb_mean = agb_mean_sd %>% purrr::map("agb_mean")%>%unlist, 
                            agb_sd = agb_mean_sd %>% purrr::map("agb_sd")%>%unlist)
  return(agb_mean_sd)
}
#' Submit jobs through `qsub` for the `GEDI_L4A_2_mean_var` function.
#' 
#' @param site_info List: list of site info including site_id, site_name, lon, and lat.
#' @param outdir Character: the physical path within which the batch job folders will be created.
#' @param which.point.in.which.file List: lists containing physical paths of GEDI tiles that intercept the each site.
#' @param num.folder Numeric: the number of batch folders to be created.
#' @param buffer Numeric: buffer distance (in degree) that is used to create the bounding box (default is 0.005 [~ 500 m]).
#' @param cores Numeric: numbers of core to be used for the parallel computation. The default is the maximum current CPU number.
#' @param prerun Character: a vector of strings that will be executed beforehand. The default is NULL.
#' 
#' @return A list containing AGB mean and standard devieation for each site.
#' @export
GEDI_L4A_2_mean_var.batch <- function(site_info, 
                                      outdir, 
                                      which.point.in.which.file, 
                                      num.folder, 
                                      buffer = 0.005, 
                                      cores = parallel::detectCores(), 
                                      prerun = NULL) {
  # report current workflow.
  PEcAn.logger::logger.info("Estimating AGB mean and uncertainty.")
  # how many files do we have.
  L <- length(which.point.in.which.file)
  # how many folders should be created.
  num.per.folder <- ceiling(L/num.folder)
  # create folder for storing job outputs.
  batch.folder <- file.path(outdir, "batch")
  # delete the whole folder if it's not empty.
  if (file.exists(batch.folder)){
    unlink(batch.folder, recursive = T)
  } 
  dir.create(batch.folder)
  folder.paths <- c()
  for (i in 1:num.folder) {
    # create folder for each set of pixels.
    head.num <- (i-1)*num.per.folder + 1
    # if the site number can not be evenly divided.
    if (i*num.per.folder > L) {
      tail.num <- L
    } else {
      tail.num <- i*num.per.folder
    }
    folder.name <- paste0("From_", head.num, "_to_", tail.num)
    folder.path <- file.path(batch.folder, folder.name)
    folder.paths <- c(folder.paths, folder.path)
    if (dir.exists(folder.path)) {
      unlink(x = file.path(folder.path, c("stderr.log", "stdout.log")))
    } else {
      dir.create(folder.path)
      # write parameters.
      configs <- list(site_info = list(site_id = site_info$site_id[head.num:tail.num],
                                       lat = site_info$lat[head.num:tail.num],
                                       lon = site_info$lon[head.num:tail.num]),
                      which.point.in.which.file = which.point.in.which.file[head.num:tail.num],
                      buffer = buffer,
                      cores = cores)
      saveRDS(configs, file = file.path(folder.path, "configs.rds"))
    }
    jobsh <- c(prerun,
               "echo \"require (purrr)",
               "       require (PEcAn.data.remote)",
               "       require (foreach)",
               "       configs <- readRDS(file.path('@FOLDER_PATH@', 'configs.rds'))",
               "       res <- GEDI_L4A_2_mean_var(configs[[1]], configs[[2]], configs[[3]], configs[[4]])",
               "       saveRDS(res, file = file.path('@FOLDER_PATH@', 'res.rds'))",
               "    \" | R --no-save")
    jobsh <- gsub("@FOLDER_PATH@", folder.path, jobsh)
    writeLines(jobsh, con = file.path(folder.path, "job.sh"))
    # qsub command.
    qsub <- "qsub -l h_rt=1:00:00 -l buyin -pe omp @CORES@ -V -N @NAME@ -o @STDOUT@ -e @STDERR@ -S /bin/bash"
    qsub <- gsub("@CORES@", cores, qsub)
    qsub <- gsub("@NAME@", paste0("Job-", i), qsub)
    qsub <- gsub("@STDOUT@", file.path(folder.path, "stdout.log"), qsub)
    qsub <- gsub("@STDERR@", file.path(folder.path, "stderr.log"), qsub)
    qsub <- strsplit(qsub, " (?=([^\"']*\"[^\"']*\")*[^\"']*$)", perl = TRUE)
    cmd <- qsub[[1]]
    out <- system2(cmd, file.path(folder.path, "job.sh"), stdout = TRUE, stderr = TRUE)
    if (tail.num == L) {
      num.folder <- i
      break
    }
  }
  PEcAn.logger::logger.info("Checking outputs.")
  l <- length(list.files(batch.folder, pattern = "res.rds", recursive = T))
  pb <- utils::txtProgressBar(min = 0, max = num.folder, style = 3)
  while(l < num.folder) {
    Sys.sleep(10)
    l <- length(list.files(batch.folder, pattern = "res.rds", recursive = T))
    utils::setTxtProgressBar(pb, l)
  }
  # assemble results.
  PEcAn.logger::logger.info("Assembling results.")
  agb <- file.path(folder.paths, "res.rds") %>% 
    purrr::map(readRDS)
  agb <- do.call(rbind, agb)
  return(agb)
}