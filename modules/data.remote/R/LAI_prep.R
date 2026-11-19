#' Prepare LAI data for the SDA workflow.
#'
#' @param site_info list: Bety list of site info including site_id, lon, and lat.
#' @param time_points character: a vector contains each time point within the start and end date.
#' @param csv.outdir character: where the final CSV file will be stored.
#' @param download.outdir character: where the LAI image files will be stored.
#' @param sensor character: the LAI product name (either `MODIS` or `VIIRS`). Default is `MODIS`.
#' @param search_window numeric: search window for locate available LAI values.
#' @param export_csv boolean: decide if we want to export the CSV file.
#' @param sd_threshold numeric or character: for filtering out any estimations with unrealistic high standard error, default is 20. The QC check will be skipped if it's set as NULL.
#' @param skip_download boolean: determine if we want to use existing LAI.csv file and skip the MODIS LAI download part.
#' @param boundary numeric vector or list: the upper and lower quantiles for filtering out noisy LAI values (e.g., c(0.05, 0.95) or list(0.05, 0.95)). The default is NULL.
#'
#' @return A data frame containing LAI and sd for each site and each time step.
#' @export
#' 
#' @author Dongchen Zhang
#' @importFrom magrittr %>%
LAI_prep <- function(site_info, time_points, csv.outdir, download.outdir, sensor = "MODIS", search_window = 30, export_csv = FALSE, sd_threshold = 20, skip_download = FALSE, boundary = NULL){
  #convert time points into paired start and end dates.
  start.end.dates <- data.frame()
  for (i in seq_along(time_points)) {
    start.end.dates <- rbind(start.end.dates,
                             list(start_date = as.character(time_points[i] - lubridate::days(search_window)),
                                  end_date = as.character(time_points[i] + lubridate::days(search_window))))
    
  }
  #grab previous data to see which site has incomplete observations, if so, download the site for the whole time period.
  #if we have previous downloaded CSV file
  if(file.exists(file.path(csv.outdir, "LAI.csv"))){
    PEcAn.logger::logger.info("Extracting previous LAI file!")
    Previous_CSV <- utils::read.csv(file.path(csv.outdir, "LAI.csv"), 
                                    colClasses = c(rep("character", 2), rep("numeric", 4), "character"))
    # filter out high uncertain LAI records.
    if (!is.null(sd_threshold)) {
      PEcAn.logger::logger.info("filtering out records with high standard errors!")
      ind.rm <- which(Previous_CSV$sd >= as.numeric(sd_threshold))
      if (length(ind.rm) > 0) {
        Previous_CSV <- Previous_CSV[-ind.rm,]
      }
    }
    LAI_Output <- matrix(NA, length(site_info$site_id), 2*length(time_points)+1) %>%
      `colnames<-`(c("site_id", paste0(time_points, "_LAI"), paste0(time_points, "_SD"))) %>%
      as.data.frame()#we need: site_id, LAI, std, target time point.
    LAI_Output$site_id <- site_info$site_id
    #Calculate LAI for each time step and site.
    #loop over time and site
    LAI.list <- time_points %>% furrr::future_map(function(t){
      out.t <- data.frame()
      for (id in site_info$site_id) {
        site_LAI <- Previous_CSV[which(Previous_CSV$site_id == id),]
        site_LAI$sd[which(site_LAI$sd<=0.66)] <- 0.66 # inflate uncertainty.
        diff_days <- abs(lubridate::days(lubridate::date(site_LAI$date)-lubridate::date(t))@day)
        if(any(diff_days <= search_window)){#data found
          out.t <- rbind(out.t, list(mean = site_LAI$lai[which.min(diff_days)], sd = site_LAI$sd[which.min(diff_days)]))
        } else {
          out.t <- rbind(out.t, list(mean = NA, sd = NA))
        }
      }
      out.t %>% purrr::set_names(c(paste0(t, "_LAI"), paste0(t, "_SD")))
    }, .progress = T)
    for (i in seq_along(time_points)) {
      t <- time_points[i]#otherwise the t will be number instead of date.
      LAI_Output[, paste0(t, "_LAI")] <- LAI.list[[i]][,paste0(t, "_LAI")]
      LAI_Output[, paste0(t, "_SD")] <- LAI.list[[i]][,paste0(t, "_SD")]
    }
  } else {#we don't have any previous downloaded CSV file.
    LAI_Output <- matrix(NA, length(site_info$site_id), 2*length(time_points)+1) %>%
      `colnames<-`(c("site_id", paste0(time_points, "_LAI"), paste0(time_points, "_SD"))) %>%
      as.data.frame()#we need: site_id, LAI, std, target time point.
    LAI_Output$site_id <- site_info$site_id
  }
  #only Site that has NA for any time points need to be downloaded.
  new_site_info <- site_info %>% purrr::map(function(x)x[!stats::complete.cases(LAI_Output)])
  #TODO: only download data for specific date when we have missing data.
  if(length(new_site_info$site_id) != 0 && !skip_download){
    # download LAI values from NASA DAAC server.
    fun <- match.fun(paste0("Prep.", sensor, ".LAI.from.DAAC"))
    # calculate terra extent based on coordinates.
    extent <- terra::ext(
      floor(min(site_info$lon, na.rm = TRUE)),
      ceiling(max(site_info$lon, na.rm = TRUE)),
      floor(min(site_info$lat, na.rm = TRUE)),
      ceiling(max(site_info$lat, na.rm = TRUE))
    )
    # Download LAI.
    for (i in seq_along(start.end.dates)) {
      LAI <- fun(site_info = new_site_info, 
                 extent = extent, 
                 from = start.end.dates[i, 1], 
                 to = start.end.dates[i, 2], 
                 download.outdir = download.outdir, 
                 csv.outdir = csv.outdir, 
                 credential_path = credential_path)
    }
    #Compare with existing CSV file. (We name the CSV file as LAI.csv)
    if(exists("Previous_CSV")){#we already read the csv file previously.
      Current_CSV <- rbind(Previous_CSV, LAI)
      #using site_id, date, and sensor to remove duplicated records.
      Current_CSV <- Current_CSV[!duplicated(paste0(Current_CSV$site_id, Current_CSV$date, Current_CSV$sensor)),]
    } else {
      Current_CSV <- LAI
    }
    # filter out noisy lai records from time series.
    if (!is.null(boundary)) {
      # unlist boundary if it's passing from the assembler function.
      if (is.list(boundary)) {
        boundary <- as.numeric(unlist(boundary))
      }
      Current_CSV <- LAI_ts_filter(Current_CSV, boundary = boundary)
    }
    # only grab LAI values from the target sensor type.
    Current_CSV <- Current_CSV[which(Current_CSV$sensor == sensor),]
    #Calculate LAI for each time step and site.
    #loop over time and site
    for (i in seq_along(time_points)) {
      t <- time_points[i]#otherwise the t will be number instead of date.
      for (id in new_site_info$site_id) {
        site_LAI <- Current_CSV[which(Current_CSV$site_id == id),]
        site_LAI$sd[which(site_LAI$sd<=0.66)] <- 0.66
        diff_days <- abs(lubridate::days(lubridate::date(site_LAI$date)-lubridate::date(t))@day)
        if(any(diff_days <= as.numeric(search_window))){#data found
          LAI_Output[which(LAI_Output$site_id==id), paste0(t, "_LAI")] <- site_LAI$lai[which.min(diff_days)]
          LAI_Output[which(LAI_Output$site_id==id), paste0(t, "_SD")] <- site_LAI$sd[which.min(diff_days)]
        }
      }
    }
  }
  PEcAn.logger::logger.info("MODIS LAI Prep Completed!")
  list(LAI_Output = LAI_Output, time_points = time_points, var = "LAI")
}
#' Prepare MODIS LAI data from the NASA DAAC server for the SDA workflow.
#'
#' @param lai.csv data frame: A data frame containing date, site id, lat, lon, lai, sd, and qc columns.
#' @param boundary numeric: A vector contains the upper and lower quantiles for filtering lai records. The default is c(0.05, 0.95).
#' 
#' @return A data frame containing date, site id, lat, lon, lai, sd, and qc columns.
#' @export
#' 
#' @author Dongchen Zhang
LAI_ts_filter <- function(lai.csv, boundary = c(0.05, 0.95)) {
  # get unique site ids.
  site.ids <- sort(unique(lai.csv$site_id))
  # loop over sites.
  rm.inds <- c()
  for (i in seq_along(site.ids)) {
    # get the lai records for the current site.
    inds <- which(lai.csv$site_id == site.ids[i])
    temp.lai <- lai.csv$lai[inds]
    # calculate the upper and lower boundaries for the current lai records.
    minmax <- stats::quantile(temp.lai, boundary)
    # find and remove outliers.
    inds.rm <- which(temp.lai < minmax[1] | temp.lai > minmax[2])
    rm.inds <- c(rm.inds, inds[inds.rm])
  }
  lai.csv <- lai.csv[-rm.inds,]
  return(lai.csv)
}
#' Prepare MODIS LAI data from the NASA DAAC server for the SDA workflow.
#'
#' @param site_info list: Bety list of site info including site_id, lon, and lat.
#' @param extent numeric: A vector contains the bounding box that covers all sites (West longitude, East longitude, South latitude ,North latitude).
#' @param from character: the start time for searching the MODIS products.
#' @param to character: the end time for searching the MODIS products.
#' @param download.outdir character: Where the MODIS tiles will be stored.
#' @param csv.outdir character: Where the final CSV file will be stored.
#' @param credential_path Character: physical path to the credential file (.netrc file).
#' 
#' @return A data frame containing LAI and sd for each site and each time step.
#' @export
#' 
#' @author Dongchen Zhang
#' @importFrom magrittr %>%
Prep.MODIS.LAI.from.DAAC <- function(site_info, extent, from, to, download.outdir, csv.outdir, credential_path) {
  # load previous CSV file.
  if (file.exists(file.path(csv.outdir, "LAI.csv"))) {
    previous.csv <- utils::read.csv(file.path(csv.outdir, "LAI.csv"), 
                                    colClasses = c("character", rep("numeric", 5), rep("character", 2)))
  } else {
    previous.csv <- NULL
  }
  # setup the foreach parallel computation.
  cl <- parallel::makeCluster(parallel::detectCores())
  doParallel::registerDoParallel(cl)
  # reproject site locations to MODIS projection.
  site.ids <- site_info$site_id
  site.locs <- cbind(site_info$lon, site_info$lat) %>%
    `colnames<-`(c("lon","lat")) %>%
    `rownames<-`(site.ids)
  # create vector based on coordinates and MODIS projection.
  pts <- data.frame(lon = site.locs[,1], lat = site.locs[,2])
  sp::coordinates(pts) <- ~lon+lat
  sp::proj4string(pts) <- sp::CRS("+proj=longlat +datum=WGS84")
  pts.reproj <- sp::spTransform(pts, "+proj=sinu +a=6371007.181 +b=6371007.181 +units=m")
  coords.reproj <- sp::coordinates(pts.reproj) %>% `colnames<-`(c("x", "y"))
  # download data.
  modis.out <- NASA_DAAC_download(ul_lat = extent[4], 
                                  ul_lon = extent[1], 
                                  lr_lat = extent[3], 
                                  lr_lon = extent[2], 
                                  from = from,
                                  to = to, 
                                  just_path = F,
                                  outdir = download.outdir,
                                  doi = "10.5067/MODIS/MCD15A3H.061",
                                  ncore = parallel::detectCores()-1,
                                  credential_path = credential_path)
  # grab id for each file.
  ids <- basename(modis.out)
  # split string.
  splits <- strsplit(x = ids, split = ".", fixed = T)
  # collect attributes table (date, ih, and iv).
  dates <- ivs <- ihs <- c()
  for (i in seq_along(ids)) {
    temp <- splits[[i]]
    # calculate dates based on year and day of year.
    y.doy <- substr(temp[[2]], 2, 8)
    year <- substr(y.doy, 1, 4)
    doy <- substr(y.doy, 5, 7)
    dates <- c(dates, as.character(as.Date(paste(year, doy), format="%Y %j")))
    # extract tile id.
    ihs <- c(ihs, substr(temp[[3]], 2, 3))
    ivs <- c(ivs, substr(temp[[3]], 5, 6))
  }
  # create data frame of extents for MODIS tiles.
  # record progress.
  PEcAn.logger::logger.info("\nCreating information table for downloaded MODIS tiles.")
  # fix github check issue.
  tile <- NULL
  MODIS.tiles.extents <- foreach::foreach(
    tile = modis.out,
    .packages=c("terra", "Kendall")
  ) %dopar% {
    return(as.vector(terra::ext(terra::rast(tile))))
  } %>% dplyr::bind_rows()
  MODIS.tiles.info <- cbind(dates, ihs, ivs, MODIS.tiles.extents)
  # filter dates.
  unique.dates <- unique(MODIS.tiles.info$dates)
  unique.dates <- unique.dates[which(as.Date(unique.dates) >= from &
                                       as.Date(unique.dates) <= to)]
  # find inds of file paths that match each point.
  PEcAn.logger::logger.info("\nFinding indexes of file paths that match each point.")
  tile.ids <- foreach::foreach(
    i = as.numeric(site.ids),
    .packages="Kendall"
  ) %dopar% {
    # filter tile position ids based on lat/lon range.
    inds <- which(as.Date(MODIS.tiles.info$dates) >= from & 
                    as.Date(MODIS.tiles.info$dates) <= to & 
                    coords.reproj[i, "x"] > MODIS.tiles.info$xmin & 
                    coords.reproj[i, "x"] < MODIS.tiles.info$xmax & 
                    coords.reproj[i, "y"] > MODIS.tiles.info$ymin &
                    coords.reproj[i, "y"] < MODIS.tiles.info$ymax)
    names(inds) <- rep(i,length(inds))
    return(inds)
  } %>% unlist
  # extract MODIS products.
  # loop over tiles.
  PEcAn.logger::logger.info("\nExtracting MODIS datasets by points.")
  unique.tile.ids <- sort(unique(tile.ids)) 
  outputs <- foreach::foreach(
    i = unique.tile.ids,
    .packages=c("purrr", "Kendall")
  ) %dopar% {
    temp.tile <- terra::rast(modis.out[i])
    points.inds <- as.numeric(names(tile.ids)[which(tile.ids == i)])
    temp.res <- terra::extract(temp.tile, data.frame(x = coords.reproj[points.inds, "x"],
                                                     y = coords.reproj[points.inds, "y"]))
    # convert QC flag.
    qc <- temp.res[,4] %>% purrr::map(function(v){
      qc_flag <- intToBits(as.integer(v)) # NB big-endian (ones place first)
      qc_flag <- as.integer(rev(utils::head(qc_flag, 3))) # now ones place last
      paste(qc_flag, collapse = "")
    }) %>% unlist
    # write into table.
    temp.res <- data.frame(date = MODIS.tiles.info$dates[i],
                           site_id = points.inds, 
                           lat = site.locs[points.inds, "lat"],
                           lon = site.locs[points.inds, "lon"],
                           lai = temp.res[,3],
                           sd = temp.res[,7],
                           qc = qc,
                           sensor = "MODIS")
    return(temp.res)
  } %>% dplyr::bind_rows()
  # stop parallel.
  parallel::stopCluster(cl)
  foreach::registerDoSEQ()
  # combine with previous CSV file.
  if (!is.null(previous.csv)) {
    outputs <- rbind(previous.csv, outputs)
    outputs <- outputs[!duplicated(outputs),]
  }
  # filter by QC band.
  outputs <- outputs[which(outputs$qc %in% c("000", "001")),]
  # write into CSV file.
  utils::write.csv(outputs, file = file.path(csv.outdir, "LAI.csv"), row.names = F)
  # delete downloaded files.
  unlink(list.files(download.outdir, full.names = T), recursive = T)
  return(outputs)
}

#' Prepare VIIRS LAI data from the NASA DAAC server for the SDA workflow.
#'
#' @param site_info list: Bety list of site info including site_id, lon, and lat.
#' @param extent numeric: A vector contains the bounding box that covers all sites (West longitude, East longitude, South latitude ,North latitude).
#' @param from character: the start time for searching the MODIS products.
#' @param to character: the end time for searching the MODIS products.
#' @param download.outdir character: Where the MODIS tiles will be stored.
#' @param csv.outdir character: Where the final CSV file will be stored.
#' @param credential_path Character: physical path to the credential file (.netrc file).
#' 
#' @return A data frame containing LAI and sd for each site and each time step.
#' @export
#' 
#' @author Dongchen Zhang
#' @importFrom magrittr %>%
Prep.VIIRS.LAI.from.DAAC <- function(site_info, extent, from, to, download.outdir, csv.outdir, credential_path) {
  # load previous CSV file.
  if (file.exists(file.path(csv.outdir, "LAI.csv"))) {
    previous.csv <- utils::read.csv(file.path(csv.outdir, "LAI.csv"), 
                                    colClasses = c("character", rep("numeric", 5), rep("character", 2)))
  } else {
    previous.csv <- NULL
  }
  # setup the foreach parallel computation.
  cl <- parallel::makeCluster(parallel::detectCores())
  doParallel::registerDoParallel(cl)
  # reproject site locations to MODIS projection.
  site.ids <- site_info$site_id
  site.locs <- cbind(site_info$lon, site_info$lat) %>%
    `colnames<-`(c("lon","lat")) %>%
    `rownames<-`(site.ids)
  # create vector based on coordinates and MODIS projection.
  pts <- data.frame(lon = site.locs[,1], lat = site.locs[,2])
  sp::coordinates(pts) <- ~lon+lat
  sp::proj4string(pts) <- sp::CRS("+proj=longlat +datum=WGS84")
  pts.reproj <- sp::spTransform(pts, "+proj=sinu +a=6371007.181 +b=6371007.181 +units=m")
  coords.reproj <- sp::coordinates(pts.reproj) %>% `colnames<-`(c("x", "y"))
  # download data.
  viirs.out <- NASA_DAAC_download(ul_lat = extent[4], 
                                  ul_lon = extent[1], 
                                  lr_lat = extent[3], 
                                  lr_lon = extent[2], 
                                  from = from,
                                  to = to, 
                                  just_path = F,
                                  outdir = download.outdir,
                                  doi = "10.5067/VIIRS/VJ215A2H.002",
                                  ncore = parallel::detectCores()-1,
                                  credential_path = credential_path)
  # grab id for each file.
  ids <- basename(viirs.out)
  # split string.
  splits <- strsplit(x = ids, split = ".", fixed = T)
  # collect attributes table (date, ih, and iv).
  dates <- ivs <- ihs <- c()
  for (i in seq_along(ids)) {
    temp <- splits[[i]]
    # calculate dates based on year and day of year.
    y.doy <- substr(temp[[2]], 2, 8)
    year <- substr(y.doy, 1, 4)
    doy <- substr(y.doy, 5, 7)
    dates <- c(dates, as.character(as.Date(paste(year, doy), format="%Y %j")))
    # extract tile id.
    ihs <- c(ihs, substr(temp[[3]], 2, 3))
    ivs <- c(ivs, substr(temp[[3]], 5, 6))
  }
  # create data frame of extents for VIIRS tiles.
  PEcAn.logger::logger.info("\nCreating information table for downloaded VIIRS tiles.")
  doSNOW::registerDoSNOW(cl)
  # fix github check issue.
  tile <- NULL
  VIIRS.tiles.extents <- foreach::foreach(
    tile = viirs.out,
    .packages=c("terra", "Kendall")
  ) %dopar% {
    ext <- grab.viirs.crs.ext(tile)$ext
    return(as.vector(terra::ext(ext)))
  } %>% dplyr::bind_rows()
  VIIRS.tiles.info <- cbind(dates, ihs, ivs, VIIRS.tiles.extents)
  # filter dates.
  unique.dates <- unique(VIIRS.tiles.info$dates)
  prop.inds <- which(as.Date(unique.dates) >= from &
                       as.Date(unique.dates) <= to)
  VIIRS.tiles.info <- VIIRS.tiles.info[prop.inds,]
  viirs.out <- viirs.out[prop.inds]
  unique.dates <- unique.dates[prop.inds]
  # setup band names.
  band.names <- c("Lai", "LaiStdDev", "FparLai_QC")
  # conversions.
  coverted.paths <- vector("list", length = length(band.names))
  for (i in seq_along(band.names)) {
    coverted.paths[[i]] <- viirs.out %>% 
      purrr::map2(seq_along(viirs.out), function(f, tile.id) {
        gdal_conversion(in_path = f, 
                        doi = "10.5067/VIIRS/VJ215A2H.002",
                        outfolder = download.outdir, 
                        band_name = band.names[i], 
                        tile_id = tile.id, 
                        just_band_name = F)
      }) %>% unlist
  }
  # find inds of file paths that match each point.
  PEcAn.logger::logger.info("\nFinding indexes of file paths that match each point.")
  tile.ids <- foreach::foreach(
    i = as.numeric(site.ids),
    .packages="Kendall"
  ) %dopar% {
    # filter tile position ids based on lat/lon range.
    inds <- which(as.Date(VIIRS.tiles.info$dates) >= from & 
                    as.Date(VIIRS.tiles.info$dates) <= to & 
                    coords.reproj[i, "x"] > VIIRS.tiles.info$xmin & 
                    coords.reproj[i, "x"] < VIIRS.tiles.info$xmax & 
                    coords.reproj[i, "y"] > VIIRS.tiles.info$ymin &
                    coords.reproj[i, "y"] < VIIRS.tiles.info$ymax)
    names(inds) <- rep(i,length(inds))
    return(inds)
  } %>% unlist
  # extract VIIRS products.
  # loop over tiles.
  PEcAn.logger::logger.info("\nExtracting VIIRS datasets by points.")
  unique.tile.ids <- sort(unique(tile.ids)) 
  outputs <- foreach::foreach(
    i = unique.tile.ids,
    .packages=c("purrr", "Kendall")
  ) %dopar% {
    # load each layer separately.
    lai <- terra::rast(coverted.paths[[1]][i])
    sd <- terra::rast(coverted.paths[[2]][i])
    qc <- terra::rast(coverted.paths[[3]][i])
    temp.tile <- c(lai, sd, qc) # Stack together.
    # extract values.
    points.inds <- as.numeric(names(tile.ids)[which(tile.ids == i)])
    temp.res <- terra::extract(temp.tile, data.frame(x = coords.reproj[points.inds, "x"],
                                                     y = coords.reproj[points.inds, "y"]))
    # convert QC flag.
    qc <- temp.res[,4] %>% purrr::map(function(v){
      qc_flag <- intToBits(as.integer(v)) # NB big-endian (ones place first)
      qc_flag <- as.integer(rev(utils::head(qc_flag, 3))) # now ones place last
      paste(qc_flag, collapse = "")
    }) %>% unlist
    # write into table.
    temp.res <- data.frame(date = VIIRS.tiles.info$dates[i],
                           site_id = points.inds, 
                           lat = site.locs[points.inds, "lat"],
                           lon = site.locs[points.inds, "lon"],
                           lai = temp.res[,2],
                           sd = temp.res[,3],
                           qc = qc,
                           sensor = "VIIRS")
    return(temp.res)
  } %>% dplyr::bind_rows()
  # stop parallel.
  parallel::stopCluster(cl)
  foreach::registerDoSEQ()
  # combine with previous CSV file.
  if (!is.null(previous.csv)) {
    outputs <- rbind(previous.csv, outputs)
    outputs <- outputs[!duplicated(outputs),]
  }
  # filter by QC band.
  outputs <- outputs[which(outputs$qc %in% c("000", "001")),]
  # write into CSV file.
  utils::write.csv(outputs, file = file.path(csv.outdir, "LAI.csv"), row.names = F)
  # delete downloaded files.
  unlink(list.files(download.outdir, full.names = T), recursive = T)
  return(outputs)
}