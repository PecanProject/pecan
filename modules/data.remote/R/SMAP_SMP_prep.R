#' Prepare SMAP Soil Moisture (SMP) data for the SDA workflow.
#'
#' @param site_info Bety list of site info including site_id, lon, and lat.
#' @param start_date Start date of SDA workflow.
#' @param end_date End date of SDA workflow.
#' @param time_points A vector contains each time point within the start and end date.
#' @param outdir Where the final CSV file, and the CSV file from GEE are stored.
#' @param search_window search window for locate available SMP values.
#' @param export_csv Decide if we want to export the CSV file.
#' @param update_csv Decide if we want to update current CSV file given an updated SMAP_gee.csv file
#'
#' @return A data frame containing SMAP smp and sd for each site and each time step.
#' @export
#'
#' @examples
#' @author Dongchen Zhang
#' @importFrom magrittr %>%
SMAP_SMP_prep <- function(site_info, start_date, end_date, time_points, 
                          outdir, search_window = 30, export_csv = TRUE, update_csv = FALSE){
  #note that, the SMAP_gee.csv file comes from Google Earth Engine (GEE) directly.
  #Code for generating this file can be found through this link: 
  #https://code.earthengine.google.com/ecbeb770e576d8ef72f72f5f12da3496
  #Feel free to contact Dongchen Zhang (zhangdc@bu.edu) who wrote this code.
  #The SMAP.csv file will be generated the first time when you use this code.
  #for the next time, it will save you lot of time if you can provide the SMAP.csv directly.
  #Initialize the multicore computation.
  if (future::supportsMulticore()) {
    future::plan(future::multicore)
  } else {
    future::plan(future::multisession)
  }
  #check if SMAP.csv exists.
  if(!file.exists(file.path(outdir, "SMAP.csv")) | as.logical(update_csv)){
    if(!file.exists(file.path(outdir, "SMAP_gee.csv"))){
      PEcAn.logger::logger.info("Please Provide SMAP dir that contains at least the SMAP_gee.csv file!")
      return(0)
    }else{
      SMAP_CSV <- utils::read.csv(file.path(outdir, "SMAP_gee.csv"))[-1,2] %>% 
        furrr::future_map(function(string){
          String <-  strsplit(gsub(",", "", gsub("\\[|\\]", "", string)), " ")[[1]]
          date <- as.Date(strsplit(String[1], "_")[[1]][5], "%Y%m%d")
          lon <- as.numeric(String[2])
          lat <- as.numeric(String[3])
          smp <- as.numeric(String[5]) * 100
          sd <- 0.04 * 100 #From Daniel
          
          #Match current lon/lat with site_info
          Longlat_matrix <- matrix(c(lon, site_info$lon, lat, site_info$lat), ncol=2)
          Distance <- sp::spDistsN1(Longlat_matrix, Longlat_matrix[1,], longlat = TRUE)[-1]
          distloc <- match(min(Distance), Distance)
          site_id <- site_info$site_id[distloc]
          list(date = date, site_id = site_id, lat = lat, lon = lon, smp = smp, sd = sd)#in date, id, lat, lon, smp, sd
        }, .progress = T) %>% dplyr::bind_rows()
      #write out csv file.
      if(as.logical((export_csv))){
        utils::write.csv(SMAP_CSV, file = file.path(outdir, "SMAP.csv"), row.names = F)
      }
    }
  }else{
    #TODO: When current SMAP.csv need to be updated
    SMAP_CSV <- utils::read.csv(file.path(outdir, "SMAP.csv"), 
                                colClasses = c(rep("character", 2), rep("numeric", 4)))
    Current_years <- sort(unique(lubridate::year(as.Date(SMAP_CSV$date))))
    Required_years <- lubridate::year(start_date):lubridate::year(end_date)
    Required_years <- Required_years[which(Required_years>=2015)] #SMAP data only available after year 2015.
    if(sum(!Required_years%in%Current_years)){
      PEcAn.logger::logger.info("The existing SMAP.csv file doesn't contain data between start and end date!")
      PEcAn.logger::logger.info("Please update the SMAP_gee.csv file to include the data that are missing! And then flag update_csv as TRUE to proceed!")
      return(0)
    }
  }
  time_points <- time_points[which(lubridate::year(time_points)>=2015)] #filter out any time points that are before 2015
  #initialize SMAP_Output
  SMAP_Output <- matrix(NA, length(site_info$site_id), 2*length(time_points)+1) %>% 
    `colnames<-`(c("site_id", paste0(time_points, "_SoilMoistFrac"), paste0(time_points, "_SD"))) %>% as.data.frame()#we need: site_id, LAI, std, target time point.
  SMAP_Output$site_id <- site_info$site_id
  #Calculate SMAP for each time step and site.
  #loop over time and site
  PEcAn.logger::logger.info("Extracting previous SMAP file!")
  SMAP.list <- time_points %>% furrr::future_map(function(t){
    out.t <- data.frame()
    for (id in site_info$site_id) {
      site_SMP <- SMAP_CSV[which(SMAP_CSV$site_id == id),]
      diff_days <- abs(lubridate::days(lubridate::date(site_SMP$date)-lubridate::date(t))@day)
      if(any(diff_days <= search_window)){#data found
        out.t <- rbind(out.t, list(mean = site_SMP$smp[which.min(diff_days)], sd = site_SMP$sd[which.min(diff_days)]))
      } else {
        out.t <- rbind(out.t, list(mean = NA, sd = NA))
      }
    }
    out.t %>% purrr::set_names(c(paste0(t, "_SoilMoistFrac"), paste0(t, "_SD")))
  }, .progress = T)
  for (i in seq_along(time_points)) {
    t <- time_points[i]#otherwise the t will be number instead of date.
    SMAP_Output[, paste0(t, "_SoilMoistFrac")] <- SMAP.list[[i]][,paste0(t, "_SoilMoistFrac")]
    SMAP_Output[, paste0(t, "_SD")] <- SMAP.list[[i]][,paste0(t, "_SD")]
  }
  PEcAn.logger::logger.info("SMAP SMP Prep Completed!")
  list(SMP_Output = SMAP_Output, time_points = time_points, var = "SoilMoistFrac")
}

#' Prepare SMAP soil moisture profile (SMP) data from the NASA DAAC server for the SDA workflow.
#' The CSV file that works for the `SMAP_SMP_prep` function will be exported.
#'
#' @param site_info list: Bety list of site info including site_id, lon, and lat.
#' @param extent numeric: A vector contains the bounding box that covers all sites (West longitude, East longitude, South latitude ,North latitude).
#' @param from character: the start time for searching the MODIS products.
#' @param to character: the end time for searching the MODIS products.
#' @param download.outdir character: Where the MODIS tiles will be stored.
#' @param csv.outdir character: Where the final CSV file will be stored.
#' 
#' @return A data frame containing SMP and sd for each site and each time step.
#' @export
#' 
#' @author Dongchen Zhang
#' @importFrom magrittr %>%
Prep.SMAP.CSV.from.DAAC <- function(site_info, extent, from, to, download.outdir, csv.outdir) {
  # SMAP CRS, EPSG:6933.
  smap.crs <- "+proj=cea +lat_ts=30 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
  # load previous CSV file.
  if (file.exists(file.path(csv.outdir, "SMAP.csv"))) {
    previous.csv <- utils::read.csv(file.path(csv.outdir, "SMAP.csv"), 
                                    colClasses = c("character", rep("numeric", 5)))
  } else {
    previous.csv <- NULL
  }
  # reproject site locations to SMAP projection.
  site.ids <- site_info$site_id
  site.locs <- cbind(site_info$lon, site_info$lat) %>%
    `colnames<-`(c("lon","lat")) %>%
    `rownames<-`(site.ids)
  pts <- data.frame(lon = site.locs[,1], lat = site.locs[,2])
  sp::coordinates(pts) <- ~lon+lat
  sp::proj4string(pts) <- sp::CRS("+proj=longlat +datum=WGS84")
  pts.reproj <- sp::spTransform(pts, smap.crs)
  coords.reproj <- sp::coordinates(pts.reproj) %>% `colnames<-`(c("x", "y"))
  # download data.
  metadata <- NASA_DAAC_download(ul_lat = extent[4], 
                                 ul_lon = extent[1], 
                                 lr_lat = extent[3], 
                                 lr_lon = extent[2], 
                                 from = from,
                                 to = to, 
                                 just_path = F,
                                 outdir = download.outdir,
                                 doi = "10.5067/LWJ6TF5SZRG3",
                                 ncore = parallel::detectCores()-1)
  smap.out <- metadata$path
  file <- smap.out[1] # select the first file.
  # grab smap extents, it's from the ArcGIS report using the SMAP H5 file.
  smap.ext <- c(-17363027.292480, 17367529.945160, -7319045.227051, 7310037.171387) %>% terra::ext()
  # convert h5 file to raster.
  smap <- terra::rast(file)
  # add extent and crs to the raster.
  terra::ext(smap) <- smap.ext#terra::ext(pr_extent)
  terra::crs(smap) <- smap.crs
  # extract values for smp and std.
  smp <- terra::extract(smap[["sm_profile_analysis"]], coords.reproj) * 100
  std <- terra::extract(smap[["sm_profile_analysis_ensstd"]], coords.reproj) * 100
  # construct final data frame.
  outputs <- c()
  for (i in seq_along(site.ids)) {
    outputs <- rbind(outputs, data.frame(date = as.character(from),
                                         site_id = i, 
                                         lat = site.locs[i, "lat"],
                                         lon = site.locs[i, "lon"],
                                         smp = smp[i,],
                                         sd = std[i,]))
  }
  # remove NAs.
  outputs <- outputs[which(!is.na(outputs$smp)),]
  # combine with previous CSV file.
  if (!is.null(previous.csv)) {
    outputs <- rbind(previous.csv, outputs)
    outputs <- outputs[!duplicated(outputs),]
  }
  # write into CSV file.
  utils::write.csv(outputs, file = file.path(csv.outdir, "SMAP.csv"), row.names = F)
  # delete downloaded files.
  unlink(list.files(download.outdir, full.names = T), recursive = T)
  return(outputs)
}