##' ocs_extract function
##' A function to extract total soil organic carbon for a single or group of 
##' lat/long locationsbased on user-defined site location from SoilGrids250m 
##' version 2.0 : https://soilgrids.org
##' @title ocs_extract
##' @name ocs_extract
##' 
##' @param site_info A dataframe of site info containing the BETYdb site ID, 
##' site name, latitude, and longitude, e.g. 
##' (site_id, site_name, lat, lon)
##' @param outdir Optional. Provide the results as an Rdata file 
##' (soilgrids_soc_data.RData)
##' @param verbose Provide progress feedback to the terminal? TRUE/FALSE
##' 
##' @export
##' @author Qianyu Li, Shawn P. Serbin
##' 
ocs_extract <- function (site_info, outdir=NULL, verbose=TRUE, ...) {
  #create a variable to store mean and quantile of organic carbon density (ocd) for each soil depth
  ocdquant <- matrix(NA, nrow = 6, ncol = length(lon_input) * 4) #row represents soil depth, col represents mean, 5%, 50% and 95%-quantile of ocd for all sites 
  #create a variable for site ID
  siteid <- matrix(NA, nrow = 6, ncol = length(lon_input) * 4)
  lonlat <- cbind(lon_input, lat_input)
  base_data_url <- "/vsicurl?max_retry=30&retry_delay=60&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/ocd/ocd_"
  depths <- c("0-5cm", "5-15cm", "15-30cm", "30-60cm", "60-100cm", "100-200cm")
 
  # reproject locations to soilgrids projection
  #Soilgrids data is using Homolosine projection https://www.isric.org/explore/soilgrids/faq-soilgrids 
  p <- terra::vect(lonlat, crs = "+proj=longlat +datum=WGS84") # Users need to provide lon/lat
  newcrs <- "+proj=igh +datum=WGS84 +no_defs +towgs84=0,0,0" 
  p_reproj <- terra::project(p, newcrs) # Transform the point vector to data with Homolosine projection
  
  # setup progress bar
  if (verbose) {
    j <- 1
    pb <- utils::txtProgressBar(min = 0, max = length(depths), char="*", width=70, style = 3)
  }
  
  for (dep in seq_along(depths)) {
    # setup virtual raster URLs for each layer
    ocd_mean.url <- paste0(base_data_url,depths[dep],"_mean.vrt")
    ocd_Q0.05.url <- paste0(base_data_url, depths[dep], "_Q0.05.vrt")
    ocd_Q0.50.url <- paste0(base_data_url, depths[dep], "_Q0.5.vrt")
    ocd_Q0.95.url <- paste0(base_data_url, depths[dep], "_Q0.95.vrt")
    
    # create virtual rasters && extract SOC values - the original unit is hg/m3
    ocd_mean <- terra::extract(terra::rast(ocd_mean.url), p_reproj)
    ocd_Q0.05_map <- terra::extract(terra::rast(ocd_Q0.05.url), p_reproj)
    ocd_Q0.50_map <- terra::extract(terra::rast(ocd_Q0.50.url), p_reproj)
    ocd_Q0.95_map <- terra::extract(terra::rast(ocd_Q0.95.url), p_reproj)
    
    #change the unit to more common kg/m3
    ocd_mean_real <- ocd_mean[, 2] / 10
    ocd_Q0.05_real <- ocd_Q0.05_map[, 2] / 10
    ocd_Q0.50_real <- ocd_Q0.50_map[, 2] / 10
    ocd_Q0.95_real <- ocd_Q0.95_map[, 2] / 10
    
    ocdquant[dep, ] <-c(ocd_mean_real,ocd_Q0.05_real,ocd_Q0.50_real,ocd_Q0.95_real)
    siteid [dep, ] <- rep(1:length(lon_input), 4)

    ### Display progress to console
    if (verbose) {
      utils::setTxtProgressBar(pb, j)
      j <- j+1
      flush.console()}
  
  # cleanup interim results
    rm(ocd_mean.url, ocd_Q0.05.url, ocd_Q0.50.url, ocd_Q0.95.url, 
       ocd_mean, ocd_Q0.05_map, ocd_Q0.50_map, ocd_Q0.95_map,
       ocd_mean_real, ocd_Q0.05_real, ocd_Q0.50_real, ocd_Q0.95_real)
  }
  
  rownames(ocdquant) <- c("0-5", "5-15", "15-30", "30-60", "60-100", "100-200")
  colnames(ocdquant) <- c(rep("Mean", length(lon_input)),
                          rep("0.05", length(lon_input)),
                          rep("0.5", length(lon_input)),
                          rep("0.95", length(lon_input)))
  rownames(siteid) <- c("0-5", "5-15", "15-30", "30-60", "60-100", "100-200")
  colnames(siteid) <- c(rep("Mean", length(lon_input)),
                        rep("0.05", length(lon_input)),
                        rep("0.5", length(lon_input)),
                        rep("0.95", length(lon_input)))
  if (verbose) {
    close(pb)
  }
  
  # parse extracted data and prepare for output
  ocd_fit <- reshape2::melt(ocdquant, id.vars = c("Mean"))
  id_fit <- reshape2::melt(siteid, id.vars = c("Mean"))
  colnames(ocd_fit) <- c("Depth", "Quantile", "Value")
  ocd_fit$Variable <- rep("ocd", length(nrow(ocd_fit)))
  ocd_fit$siteid <- id_fit$value
  dat <- split(ocd_fit, list(ocd_fit$siteid, ocd_fit$Depth))
  
  #assume the ocd profile follows gamma distribution best
  cgamma <- function(theta, val, stat) {
    pred <- rep(NA, 4)
    names(pred) = stat
    if ("Mean" %in% stat) {
      pred["Mean"] <- theta[1] / theta[2]
    }
    qstat <- as.numeric(stat)[!is.na(as.numeric(stat))]
    pred[as.character(qstat)] <- qgamma(qstat, theta[1], theta[2])
    return(sum((pred - val) ^ 2))
  }
  
  fitQ <- function(x) {
    val = x$Value
    stat = as.character(x$Quantile)
    theta = c(10, 10)
    fit <-
      list(Gamma = optim(theta, cgamma, val = val, stat = stat))
    SS <- sapply(fit, function(f) {
      f$value
    })
    par <- sapply(fit, function(f) {
      f$par
    })
    return(list(par = par, SS = SS))
  }
  
  score <- suppressWarnings(lapply(dat, fitQ))
  bestPar <- sapply(score, function(f) { f$par })
  mean <- bestPar[1,] / bestPar[2,]
  std <- sqrt(bestPar[1,] / bestPar[2,] ^ 2)
  mean_site <- matrix(mean, length(lon_input), 6)
  rownames(mean_site) <- paste0("Site_", 1:length(lon_input))
  colnames(mean_site) <- c("0-5cm",
                           "5-15cm",
                           "15-30cm",
                           "30-60cm",
                           "60-100cm",
                           "100-200cm")
  std_site <- matrix(std, length(lon_input), 6)
  rownames(std_site) <- paste0("Site_", 1:length(lon_input))
  colnames(std_site) <- c("0-5cm",
                          "5-15cm",
                          "15-30cm",
                          "30-60cm",
                          "60-100cm",
                          "100-200cm")
  #calculate organic carbon stock (ocs) as the sum of organic carbon density multiplied by layer thickness, the unit of ocs is kg/m2, based on Eq. (6）in paper https://www.sciencedirect.com/science/article/pii/S2215016122000462
  ocs_sum <- mean_site[,1]*(5-0)*0.01+mean_site[,2]*(15-5)*0.01+mean_site[,3]*(30-15)*0.01+mean_site[,4]*(60-30)*0.01+mean_site[,5]*(100-60)*0.01+mean_site[,6]*(200-100)*0.01 
  #calculate standard deviation of ocs as the square root of sum of variance of layer-specific ocs, the unit of ocs is kg/m2, based on Eq. (8) in paper https://www.sciencedirect.com/science/article/pii/S2215016122000462, except the correlation term due to the lack of information 
  ocs_std <- sqrt((std_site[,1]*(5-0)*0.01)^2+(std_site[,2]*(15-5)*0.01)^2+(std_site[,3]*(30-15)*0.01)^2+(std_site[,4]*(60-30)*0.01)^2+(std_site[,5]*(100-60)*0.01)^2+(std_site[,6]*(200-100)*0.01)^2)
  
  if (!is.null(outdir)) {
    PEcAn.logger::logger.info(paste0("Storing results in: ",file.path(outdir,"soilgrids_soc_data.RData")))
    if (! file.exists(outdir)) dir.create(outdir,recursive=TRUE)
    soilgrids_soc_data <- list("Total_soc" = ocs_sum, "Sdev_soc" = ocs_std)
    save(soilgrids_soc_data, file = file.path(outdir,"soilgrids_soc_data.RData"))
  }
  # return the results to the terminal as well
  return(list("Total OCS" = ocs_sum, "Standard deviation of OCS" = ocs_std))
}

