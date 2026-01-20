##' soilgrids_texture_extraction function
##' A function to extract and save three types of soil texture data in parallel for a single or group of 
##' lat/long locations based on user-defined site location from SoilGrids250m 
##' version 2.0 : https://soilgrids.org
##' @title soilgrids_texture_extraction
##' @name soilgrids_texture_extraction
##' 
##' @param data_paths A list containing the data (either virtual raster files or local file folders) and output path for all types of SoilGrids texture data
##'  e.g. data_paths <- list(
##'   list(
##'      url = "/vsicurl?max_retry=30&retry_delay=60&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/sand/sand_",
##'      local = NULL,
##'      save_path = paste0(outdir, "sand_percent.rds")),
##'   list(
##'      url = "/vsicurl?max_retry=30&retry_delay=60&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/clay/clay_",
##'      local = NULL,
##'      save_path = paste0(outdir, "clay_percent.rds")),
##'   list(
##'      url = "/vsicurl?max_retry=30&retry_delay=60&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/silt/silt_",
##'      local = NULL,
##'      save_path = paste0(outdir, "silt_percent.rds")))

##' @param site_info A data frame of site info containing the BETYdb site ID, 
##' site name, latitude, and longitude, e.g. 
##' (site_id, lat, lon)
##' @param outdir  Provide the path to store the texture data file
##' @param verbose Provide progress feedback to the terminal? TRUE/FALSE
##' @return a data frame containing the soil texture data with columns "Depth", "Quantile", "Siteid", and "Value"
##' 
##' @export
##' @author Qianyu Li
##' @importFrom magrittr %>%

soilgrids_texture_extraction <- function(data_paths, site_info, outdir=NULL, verbose=TRUE){
  
  # A function to extract and save one type of soil texture data
  download_and_extraction <- function(base_data, site_info) {
    #choose between virtual raster files or local files
    if (!is.null (base_data$url)) {
      vrt.flag <- TRUE
    } else {
      vrt.flag <- FALSE
    }
    if (is.null(site_info)) {
      PEcAn.logger::logger.error(
        "No site information found. Please provide a BETY DB site list containing at least the site id and PostGIS geometry\
    as lon and lat"
      )
    }
    
    # Prepare site info for extraction
    internal_site_info <- data.frame(site_info$site_id,site_info$lat,site_info$lon)
    #Create a variable to store mean and quantile of soil texture data for each soil depth
    soiltquant <- matrix(NA, nrow = 6, ncol = length(internal_site_info$site_info.lon) * 4)
    lonlat <-cbind(internal_site_info$site_info.lon, internal_site_info$site_info.lat)
    depths <-c("0-5cm", "5-15cm", "15-30cm", "30-60cm", "60-100cm", "100-200cm")
    
    p <-terra::vect(lonlat, crs = "+proj=longlat +datum=WGS84") # the projection for sites with lon/lat is WGS84
    if (vrt.flag) {
       # Reproject locations to soilgrids projection
       # Soilgrids data is using Homolosine projection https://www.isric.org/explore/soilgrids/faq-soilgrids
        newcrs <- "+proj=igh +datum=WGS84 +no_defs +towgs84=0,0,0"
        p_reproj <- terra::project(p, newcrs) # Transform the point vector to data with Homolosine projection
        data_tag <- c("_mean.vrt", "_Q0.05.vrt", "_Q0.5.vrt", "_Q0.95.vrt")
       } else {
        data_tag <- c("_mean.tif", "_Q0.05.tif", "_Q0.5.tif", "_Q0.95.tif")  
       }
     name_tag <- expand.grid(depths, data_tag, stringsAsFactors = F) #find the combinations between data and depth tags.
     L <- split(as.data.frame(name_tag), seq(nrow(as.data.frame(name_tag))))#convert tags into lists.
     soilt_real <- vector("list", length = length(L))
     pb <- utils::txtProgressBar(min = 0, max = length(L), style = 3)
     for (i in seq_along(L)) {
        l <- L[[i]]
        if (vrt.flag) {
          soilt <- paste0(base_data$url, l[[1]], l[[2]]) #e.g. "/vsicurl?max_retry=30&retry_delay=60&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/sand/sand_0-5cm_mean.vrt"
          soilt_map <- terra::extract(terra::rast(soilt), p_reproj)
        } else {
          #assume the projection of tif file is already WGS84
          soilt <- paste0(base_data$local, l[[1]], l[[2]])
          soilt_map <- terra::extract(terra::rast(soilt), p)
        }
          soilt_real[[i]] <- unlist(soilt_map[, -1])/10
          utils::setTxtProgressBar(pb, i)
        }
    
     for (dep in seq_along(depths)) {
         dep.ind <- which(grepl(depths[dep], name_tag[, 1]))
         soiltquant[dep, ] <- soilt_real[dep.ind] %>% unlist
       }
    
    
     # Parse extracted data and prepare for output
     quantile_name <-c(paste("Mean_", site_info$site_id, sep = ""),paste("0.05_", site_info$site_id, sep = ""),paste("0.5_", site_info$site_id, sep = ""),paste("0.95_", site_info$site_id, sep = ""))
     colnames(soiltquant) <- quantile_name
     soilt_dep <- cbind(soiltquant, depths)
     soilt_df <- tidyr::pivot_longer(as.data.frame(soilt_dep),cols = tidyselect::all_of(quantile_name),names_to = c("Quantile", "Siteid"),names_sep = "_")
     # Remove NA
     soilt_df <- stats::na.omit(soilt_df)
     colnames(soilt_df) <- c("Depth", "Quantile", "Siteid", "Value")
     soilt_df$Value<-as.numeric(soilt_df$Value)
    
     if (!is.null(base_data$save_path)) {
        PEcAn.logger::logger.info(paste0("Storing results in: ",base_data$save_path))
        saveRDS(soilt_df,file=base_data$save_path)
     }
     else {
        PEcAn.logger::logger.error("No output directory found.")
     }
     # Return the results to the terminal as well
     return(soilt_df)
  }
  
  
  if (future::supportsMulticore()) {
    future::supportsMulticore()
    future::plan(future::multicore)
   } else {
    future::plan(future::multisession,workers=3)
  }
  
  soil_text <-furrr::future_map(data_paths,function(source){
    download_and_extraction(source, site_info)
  })
}

