##' @title soilgrids.soc.extract
##' @name  soilgrids.soc.extract
##' 
##' @param site_info A BETYdb site info dataframe containing at least each site ID, sitename, 
##' latitude, longitude, and time_zone. e.g. c(site_qry$id, site_qry$sitename, site_qry$lon,
##' site_qry$lat, site_qry$time_zone)
##' @param verbose logical. If TRUE show the extraction progress bar
##' 
##' ##' @examples
##' \dontrun{
##' 
##' # Example 1 - using the modex.bnl.gov BETYdb and site IDs to extract data
##' db <- 'betydb'
##' host_db <- 'modex.bnl.gov'
##' db_port <- '5432'
##' db_user <- 'bety'
##' db_password <- 'bety'
##' 
##' bety <- list(user='bety', password='bety', host='modex.bnl.gov',
##' dbname='betydb', driver=RPostgres::Postgres(),write=FALSE)
##' 
##' con <- DBI::dbConnect(drv=bety$driver, dbname=bety$dbname, host=bety$host, 
##' password=bety$password, user=bety$user)
##' 
##' suppressWarnings(site_qry <- glue::glue_sql("SELECT *, ST_X(ST_CENTROID(geometry)) AS lon,
##' ST_Y(ST_CENTROID(geometry)) AS lat FROM sites WHERE id IN ({ids*})",
##' ids = c("676","622","678","766","764"), .con = con))
##' 
##' suppressWarnings(qry_results.1 <- DBI::dbSendQuery(con,site_qry))
##' suppressWarnings(qry_results.2 <- DBI::dbFetch(qry_results.1))
##' DBI::dbClearResult(qry_results.1)
##' dbDisconnect(con)
##' 
##' site_info <- qry_results.2
##' verbose <- TRUE
##' system.time(result_soc <- soc_extract(site_info=site_info, verbose=verbose))
##' result_soc
##' 
##' }
##' 
##' @importFrom reshape2 melt
##' @importFrom terra vect project rast extract
##' @importFrom stats qgamma optim
##' @importFrom utils flush.console setTxtProgressBar txtProgressBar
##' 
##' @return list of two dataframes containing the mean SOC values per location and depth
##' and the corresponding standard deviation values (uncertainties) for each location 
##' and depth. Output column names are c("site_id","lat","lon","0-5cm","5-15cm","15-30cm",
##' "30-60cm","60-100cm","100-200cm")
##' 
##' @export
##' @author Qianyu Li, Shawn P. Serbin
##' 
soilgrids.soc.extract <- function (site_info=NULL, verbose=TRUE) {
  
  
  if (is.null(site_info)) {
    stop("Please provide a BETY DB site list containing at least the site id and PostGIS geometry\
    as lon and lat")
  }
  
  # prepare site info for extraction
  internal_site_info <- data.frame(site_info$id, site_info$sitename, site_info$lat, 
                                   site_info$lon, site_info$time_zone)
  
  i <- 1 # setup counter
  #create a variable to store mean and quantile of SOC for each soil depth
  socquant <- matrix(NA, nrow = 6, ncol = length(internal_site_info$site_info.lon) * 4)
  siteids <- matrix(NA, nrow = 6, ncol = length(internal_site_info$site_info.id) * 4)
  lonlat <- cbind(internal_site_info$site_info.lon, internal_site_info$site_info.lat)
  base_data_url <- "/vsicurl?max_retry=30&retry_delay=60&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/soc/soc_"
  depths <- c("0-5cm", "5-15cm", "15-30cm", "30-60cm", "60-100cm", "100-200cm")
  
  # reproject locations to soilgrids projection
  #Soilgrids data is using Homolosine projection https://www.isric.org/explore/soilgrids/faq-soilgrids
  p <- terra::vect(lonlat, crs = "+proj=longlat +datum=WGS84") # Users need to provide lon/lat
  newcrs <- "+proj=igh +datum=WGS84 +no_defs +towgs84=0,0,0" 
  p_reproj <- terra::project(p, newcrs) # Transform the point vector to data with Homolosine projection
  
  # setup progress bar
  if (verbose) {
    j <- 1
    pb <- txtProgressBar(min = 0, max = length(depths), char="*", width=70, style = 3)
  }
  
  
  for (dep in seq_along(depths)) {
    
    # ---- could also replace using terra with just raster and extract similar to the landtrendr functions
    # setup virtual raster URLs for each layer
    soc_mean.url <- paste0(base_data_url, depths[dep],"_mean.vrt")
    soc_Q0.05.url <- paste0(base_data_url, depths[dep], "_Q0.05.vrt")
    soc_Q0.50.url <- paste0(base_data_url, depths[dep], "_Q0.5.vrt")
    soc_Q0.95.url <- paste0(base_data_url, depths[dep], "_Q0.95.vrt")
    
    # create virtual rasters && extract SOC values - the original unit is dg/kg
    soc_mean <- terra::extract(terra::rast(soc_mean.url), p_reproj)
    soc_Q0.05_map <- terra::extract(terra::rast(soc_Q0.05.url), p_reproj)
    soc_Q0.50_map <- terra::extract(terra::rast(soc_Q0.50.url), p_reproj)
    soc_Q0.95_map <- terra::extract(terra::rast(soc_Q0.95.url), p_reproj)
    # ---- could also replace using terra with just raster and extract similar to the landtrendr functions
    
    #change the unit to more common g/kg
    soc_mean_real <- soc_mean[, 2] / 10
    soc_Q0.05_real <- soc_Q0.05_map[, 2] / 10
    soc_Q0.50_real <- soc_Q0.50_map[, 2] / 10
    soc_Q0.95_real <- soc_Q0.95_map[, 2] / 10
    
    socquant[i,] <- c(soc_mean_real, soc_Q0.05_real, soc_Q0.50_real, soc_Q0.95_real)
    siteids[i,] <- rep(as.numeric(internal_site_info$site_info.id), 4)
    
    rownames(socquant) <- c("0-5", "5-15", "15-30", "30-60", "60-100", "100-200")
    colnames(socquant) <- c(rep("Mean", length(internal_site_info$site_info.lon)), 
                            rep("0.05", length(internal_site_info$site_info.lon)),
                            rep("0.5", length(internal_site_info$site_info.lon)),
                            rep("0.95", length(internal_site_info$site_info.lon)))
    rownames(siteids) <- c("0-5", "5-15", "15-30", "30-60", "60-100", "100-200")
    colnames(siteids) <- c(rep("Mean", length(internal_site_info$site_info.lon)), 
                           rep("0.05", length(internal_site_info$site_info.lon)), 
                           rep("0.5", length(internal_site_info$site_info.lon)), 
                           rep("0.95", length(internal_site_info$site_info.lon)))
    i <- i + 1
    
    ### Display progress to console
    if (verbose) {
      setTxtProgressBar(pb, j)
      j <- j+1
      flush.console()
    }
    # cleanup interim results
    rm(soc_mean.url, soc_Q0.05.url, soc_Q0.50.url, soc_Q0.95.url, 
       soc_mean, soc_Q0.05_map, soc_Q0.50_map, soc_Q0.95_map,
       soc_mean_real, soc_Q0.05_real, soc_Q0.50_real, soc_Q0.95_real)
  }
  
  if (verbose) {
    close(pb)
  }
  
  # parse extracted data and prepare for output
  soc_fit <- melt(socquant, id.vars = c("Mean"))
  id_fit <- melt(siteids, id.vars = c("Mean"))
  colnames(soc_fit) <- c("Depth", "Quantile", "Value")
  soc_fit$Variable <- rep("soc", length(nrow(soc_fit)))
  soc_fit$siteids <- id_fit$value
  dat <- split(soc_fit, list(soc_fit$siteids, soc_fit$Depth))
  
  #assume the soc profile follows gamma distribution best
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
  std <- sqrt(bestPar[1,] / bestPar[2,]^2)
  mean_site <- matrix(mean, length(internal_site_info$site_info.lon), 6)
  rownames(mean_site) <- as.numeric(internal_site_info$site_info.id)
  colnames(mean_site) <- c("0-5cm",
                           "5-15cm",
                           "15-30cm",
                           "30-60cm",
                           "60-100cm",
                           "100-200cm")
  mean_site.2 <- data.frame(site_id=internal_site_info$site_info.id, 
                            lat=internal_site_info$site_info.lat, 
                            lon=internal_site_info$site_info.lon, 
                            mean_site)
  colnames(mean_site.2)[4:9] <- c("0-5cm",
                                  "5-15cm",
                                  "15-30cm",
                                  "30-60cm",
                                  "60-100cm",
                                  "100-200cm")
  std_site <- matrix(std, length(internal_site_info$site_info.lon), 6)
  rownames(std_site) <- as.numeric(internal_site_info$site_info.id)
  colnames(std_site) <- c("0-5cm",
                          "5-15cm",
                          "15-30cm",
                          "30-60cm",
                          "60-100cm",
                          "100-200cm")
  std_site.2 <- data.frame(site_id=internal_site_info$site_info.id, 
                           lat=internal_site_info$site_info.lat, 
                           lon=internal_site_info$site_info.lon, 
                           std_site)
  colnames(std_site.2)[4:9] <- c("0-5cm",
                                 "5-15cm",
                                 "15-30cm",
                                 "30-60cm",
                                 "60-100cm",
                                 "100-200cm")
  return(list("Mean_soc" = mean_site.2, "Sdev_soc" = std_site.2))
} ### end of function