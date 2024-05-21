##' soilgrids_soilC_extract function
##' A function to extract total soil organic carbon for a single or group of 
##' lat/long locationsbased on user-defined site location from SoilGrids250m 
##' version 2.0 : https://soilgrids.org
##' @title soilgrids_soilC_extract
##' @name soilgrids_soilC_extract
##' 
##' @param site_info A dataframe of site info containing the BETYdb site ID, 
##' site name, latitude, and longitude, e.g. 
##' (site_id, site_name, lat, lon)
##' @param outdir Optional. Provide the results as a CSV file 
##' (soilgrids_soilC_data.csv)
##' @param verbose Provide progress feedback to the terminal? TRUE/FALSE
##' 
##' @examples
##' \dontrun{
##' 
##' # Example 1 - using the modex.bnl.gov BETYdb and site IDs to extract data
##' db <- 'betydb'
##' host_db <- 'modex.bnl.gov'
##' db_port <- '5432'
##' db_user <- 'bety'
##' db_password <- 'bety'
##' 
##' bety <- list(user='bety', password='bety', host=host_db,
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
##' DBI::dbDisconnect(con)
##' 
##' site_info <- qry_results.2
##' verbose <- TRUE
##' system.time(result_soc <- PEcAn.data.land::soilgrids_soilC_extract(site_info=site_info, 
##' verbose=verbose))
##' result_soc
##' 
##' }

##' @return a dataframe containing the total soil carbon values  
##' and the corresponding standard deviation values (uncertainties) for each location 
##' Output column names are c("Site_ID","Site_Name","Latitude","Longitude",
##' "Total_soilC","Std_soilC")
##' 
##' @export
##' @author Qianyu Li, Shawn P. Serbin
##' @importFrom magrittr %>%
soilgrids_soilC_extract <- function (site_info, outdir=NULL, verbose=TRUE) {
  if (future::supportsMulticore()) {
    future::plan(future::multicore)
  } else {
    future::plan(future::multisession)
  }
  if (is.null(site_info)) {
  PEcAn.logger::logger.error("No site information found. Please provide a BETY DB site list containing at least the site id and PostGIS geometry\
    as lon and lat")
  }
  
  # prepare site info for extraction
  internal_site_info <- data.frame(site_info$site_id, site_info$site_name, site_info$lat,site_info$lon)
  #create a variable to store mean and quantile of organic carbon density (ocd) for each soil depth
  ocdquant <- matrix(NA, nrow = 6, ncol = length(internal_site_info$site_info.lon) * 4) #row represents soil depth, col represents mean, 5%, 50% and 95%-quantile of ocd for all sites 
  lonlat <- cbind(internal_site_info$site_info.lon, internal_site_info$site_info.lat)
  base_data_url <- "/vsicurl?max_retry=30&retry_delay=60&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/ocd/ocd_"
  depths <- c("0-5cm", "5-15cm", "15-30cm", "30-60cm", "60-100cm", "100-200cm")
  layer_thick <- c(0.05,0.10,0.15,0.30,0.40,1.00) # in unit m
 
  # reproject locations to soilgrids projection
  #Soilgrids data is using Homolosine projection https://www.isric.org/explore/soilgrids/faq-soilgrids 
  p <- terra::vect(lonlat, crs = "+proj=longlat +datum=WGS84") # Users need to provide lon/lat
  newcrs <- "+proj=igh +datum=WGS84 +no_defs +towgs84=0,0,0" 
  p_reproj <- terra::project(p, newcrs) # Transform the point vector to data with Homolosine projection
  data_tag <- c("_mean.vrt", "_Q0.05.vrt", "_Q0.5.vrt", "_Q0.95.vrt")
  name_tag <- expand.grid(depths, data_tag, stringsAsFactors = F)#find the combinations between data and depth tags.
  L <- split(as.data.frame(name_tag), seq(nrow(as.data.frame(name_tag))))#convert tags into lists.
  if ("try-error" %in% class(try(ocd_real <- L %>% furrr::future_map(function(l){
    ocd_url <- paste0(base_data_url, l[[1]], l[[2]])
    ocd_map <- terra::extract(terra::rast(ocd_url), p_reproj)
    unlist(ocd_map[, -1])/10
  }, .progress = T)))) {
    ocd_real <- vector("list", length = length(L))
    pb <- utils::txtProgressBar(min = 0, max = length(L), style = 3)
    for (i in seq_along(L)) {
      l <- L[[i]]
      ocd_url <- paste0(base_data_url, l[[1]], l[[2]])
      ocd_map <- terra::extract(terra::rast(ocd_url), p_reproj)
      ocd_real[[i]] <- unlist(ocd_map[, -1])/10
      utils::setTxtProgressBar(pb, i)
    }
  }
  
  for (dep in seq_along(depths)) {
    dep.ind <- which(grepl(depths[dep], name_tag[, 1]))
    ocdquant[dep, ] <- ocd_real[dep.ind] %>% unlist
  }
  na.ind <- which(is.na(ocdquant[1, 1:length(site_info$site_id)]))
  internal_site_info <- data.frame(site_info$site_id[-na.ind], 
                                   site_info$site_name[-na.ind], 
                                   site_info$lat[-na.ind],
                                   site_info$lon[-na.ind]) %>% `colnames<-`(names(site_info))
  
 # parse extracted data and prepare for output
  quantile_name <-c(paste("Mean_",site_info$site_id,sep=""),paste("0.05_",site_info$site_id,sep=""),paste("0.5_",site_info$site_id,sep=""),paste("0.95_",site_info$site_id,sep=""))
  colnames(ocdquant) <- quantile_name
  ocdquant_dep <- cbind(ocdquant,depths)
  ocd_df <- tidyr::pivot_longer(as.data.frame(ocdquant_dep),cols=tidyselect::all_of(quantile_name),names_to=c("Quantile", "Siteid"),names_sep = "_")
  #remove NA from ocd_df
  ocd_df <- stats::na.omit(ocd_df)
  colnames(ocd_df) <- c("Depth","Quantile", "Siteid","Value")
  ocd_df$Value<-as.numeric(ocd_df$Value)
  f1<-factor(ocd_df$Siteid,levels=unique(ocd_df$Siteid))
  f2<-factor(ocd_df$Depth,levels=unique(ocd_df$Depth))
  #split data by groups of sites and soil depth, while keeping the original order of each group
  dat <- split(ocd_df, list(f1, f2))  
  
  #assume the ocd profile follows gamma distribution best
  cgamma <- function(theta, val, stat) {
    pred <- rep(NA, 4)
    names(pred) = stat
    if ("Mean" %in% stat) {
      pred["Mean"] <- theta[1] / theta[2]
    }
    qstat <- as.numeric(stat)[!is.na(as.numeric(stat))]
    pred[as.character(qstat)] <- stats::qgamma(qstat, theta[1], theta[2])
    return(sum((pred - val) ^ 2))
  }
  
  fitQ <- function(x) {
    val = x$Value
    stat = as.character(x$Quantile)
    theta = c(10, 10)
    fit <-
      list(Gamma = stats::optim(theta, cgamma, val = val, stat = stat))
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
  mean_site <- matrix(mean, length(internal_site_info$lon), 6)
  rownames(mean_site) <- as.numeric(internal_site_info$site_id)
  colnames(mean_site) <- depths
  mean_site.2 <- data.frame(site_id=internal_site_info$site_id, 
                            lat=internal_site_info$lat, 
                            lon=internal_site_info$lon, 
                            mean_site)
  colnames(mean_site.2)[4:9] <-  depths 

  std_site <- matrix(std, length(internal_site_info$lon), 6)
  rownames(std_site) <- as.numeric(internal_site_info$site_id)
  colnames(std_site) <- depths
  std_site.2 <- data.frame(site_id=internal_site_info$site_id,  
                            lat=internal_site_info$lat,
                            lon=internal_site_info$lon, 
                            std_site)
  colnames(std_site.2)[4:9] <-  depths 
  #calculate organic carbon stock (ocs) as the sum of organic carbon density multiplied by layer thickness, the unit of ocs is kg/m2, based on Eq. (6）in paper https://www.sciencedirect.com/science/article/pii/S2215016122000462
  ocs_sum <- mean_site[,1]*layer_thick[1]+mean_site[,2]*layer_thick[2]+mean_site[,3]*layer_thick[3]+mean_site[,4]*layer_thick[4]+mean_site[,5]*layer_thick[5]+mean_site[,6]*layer_thick[6] 
  #calculate standard deviation of ocs as the square root of sum of variance of layer-specific ocs, the unit of ocs is kg/m2, based on Eq. (8) in paper https://www.sciencedirect.com/science/article/pii/S2215016122000462, except the correlation term due to the lack of information 
  ocs_std <- sqrt((std_site[,1]*layer_thick[1])^2+(std_site[,2]*layer_thick[2])^2+(std_site[,3]*layer_thick[3])^2+(std_site[,4]*layer_thick[4])^2+(std_site[,5]*layer_thick[5])^2+(std_site[,6]*layer_thick[6])^2)
  ocs_sum_30cm <- mean_site[,1]*layer_thick[1]+mean_site[,2]*layer_thick[2]+mean_site[,3]*layer_thick[3]
  ocs_std_30cm <- sqrt((std_site[,1]*layer_thick[1])^2+(std_site[,2]*layer_thick[2])^2+(std_site[,3]*layer_thick[3])^2)
  soilgrids_soilC_data <- data.frame(internal_site_info$site_id,
                                     internal_site_info$site_name,
                                     internal_site_info$lat,
                                     internal_site_info$lon,
                                     ocs_sum,ocs_std,
                                     ocs_sum_30cm,
                                     ocs_std_30cm)
  colnames(soilgrids_soilC_data)<- c("Site_ID","Site_Name","Latitude","Longitude","Total_soilC_0-200cm","Std_soilC_0-200cm","Total_soilC_0-30cm","Std_soilC_0-30cm")
  rownames(soilgrids_soilC_data) <- NULL

  if (!is.null(outdir)) {
    PEcAn.logger::logger.info(paste0("Storing results in: ",file.path(outdir,"soilgrids_soilC_data.csv")))
    utils::write.csv(soilgrids_soilC_data,file=file.path(outdir,"soilgrids_soilC_data.csv"),row.names = FALSE)
  }
  else {
    PEcAn.logger::logger.error("No output directory found.")
  }
  # return the results to the terminal as well
  return(soilgrids_soilC_data)
}