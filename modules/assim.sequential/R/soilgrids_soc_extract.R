###Extraction of total soil organic carbon based on user-defined site location from SoilGrids250m version 2.0 : https://soilgrids.org
###Input parameter: user-defined site longitude/latitude 
###Extract variable ocd (organic carbon density)
###Example:ocs<-ocs_extract(c(-77.904, -119.1944, -121.557), c(40.6658, 37.0678, 44.4524))

rm(list = ls())
library(terra)
library(reshape2)
library(dplyr)

ocs_extract <- function (lon_input, lat_input) {
  i <- 1
  #create a variable to store mean and quantile of organic carbon density (ocd) for each soil depth
  ocdquant <- matrix(NA, nrow = 6, ncol = length(lon_input) * 4) #row represents soil depth, col represents mean, 5%, 50% and 95%-quantile of ocd for all sites 
  #create a variable for site ID
  siteid <- matrix(NA, nrow = 6, ncol = length(lon_input) * 4)
  lonlat <- cbind(lon_input, lat_input)
  for (dep in c("0-5cm",
                "5-15cm",
                "15-30cm",
                "30-60cm",
                "60-100cm",
                "100-200cm")) {
    ocd_mean.url <-
      paste0(
        "/vsicurl?max_retry=30&retry_delay=60&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/ocd/ocd_",
        dep,
        "_mean.vrt"
      )
    
    ocd_Q0.05.url <-
      paste0(
        "/vsicurl?max_retry=30&retry_delay=60&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/ocd/ocd_",
        dep,
        "_Q0.05.vrt"
      )
    
    ocd_Q0.50.url <-
      paste0(
        "/vsicurl?max_retry=30&retry_delay=60&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/ocd/ocd_",
        dep,
        "_Q0.5.vrt"
      )
    
    ocd_Q0.95.url <-
      paste0(
        "/vsicurl?max_retry=30&retry_delay=60&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/ocd/ocd_",
        dep,
        "_Q0.95.vrt"
      )
    
    ocd_mean_ras <- terra::rast(ocd_mean.url)
    ocd_Q0.05_ras <- terra::rast(ocd_Q0.05.url)
    ocd_Q0.50_ras <- terra::rast(ocd_Q0.50.url)
    ocd_Q0.95_ras <- terra::rast(ocd_Q0.95.url)
    
    p <-
      terra::vect(lonlat, crs = "+proj=longlat +datum=WGS84") # Users need to provide lon/lat
    newcrs <-
      "+proj=igh +datum=WGS84 +no_defs +towgs84=0,0,0" #Soilgrids data is using Homolosine projection https://www.isric.org/explore/soilgrids/faq-soilgrids
    p_rob <-
      terra::project(p, newcrs) # Transform the point vector to data with Homolosine projection
    
    #extract ocd values
    ocd_mean <- terra::extract(ocd_mean_ras, p_rob) #the original unit is hg/m3
    ocd_Q0.05 <- terra::extract(ocd_Q0.05_ras, p_rob)
    ocd_Q0.50 <- terra::extract(ocd_Q0.50_ras, p_rob)
    ocd_Q0.95 <- terra::extract(ocd_Q0.95_ras, p_rob)
    
    #change the unit to more common kg/m3
    ocd_mean_real <- ocd_mean[, 2] / 10
    ocd_Q0.05_real <- ocd_Q0.05[, 2] / 10
    ocd_Q0.50_real <- ocd_Q0.50[, 2] / 10
    ocd_Q0.95_real <- ocd_Q0.95[, 2] / 10
    
    ocdquant[i, ] <-
      c(ocd_mean_real,
        ocd_Q0.05_real,
        ocd_Q0.50_real,
        ocd_Q0.95_real)
    
    siteid [i, ] <- rep(1:length(lon_input), 4)
    rownames(ocdquant) <-
      c("0-5", "5-15", "15-30", "30-60", "60-100", "100-200")
    colnames(ocdquant) <-
      c(
        rep("Mean", length(lon_input)),
        rep("0.05", length(lon_input)),
        rep("0.5", length(lon_input)),
        rep("0.95", length(lon_input))
      )
    rownames(siteid) <-
      c("0-5", "5-15", "15-30", "30-60", "60-100", "100-200")
    colnames(siteid) <-
      c(
        rep("Mean", length(lon_input)),
        rep("0.05", length(lon_input)),
        rep("0.5", length(lon_input)),
        rep("0.95", length(lon_input))
      )
    i <- i + 1
  }
  
  ocd_fit <- melt(ocdquant, id.vars = c("Mean"))
  id_fit <- melt(siteid, id.vars = c("Mean"))
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
  
  score <- lapply(dat, fitQ)
  bestPar <- sapply(score, function(f) {
    f$par
  })
  mean <- bestPar[1,] / bestPar[2,]
  std <- sqrt(bestPar[1,] / bestPar[2,] ^ 2)
  mean_site <- matrix(mean, length(lon_input), 6)
  std_site <- matrix(std, length(lon_input), 6)
  rownames(mean_site) <- paste0("Site_", 1:length(lon_input))
  colnames(mean_site) <- c("0-5cm",
                           "5-15cm",
                           "15-30cm",
                           "30-60cm",
                           "60-100cm",
                           "100-200cm")
  rownames(std_site) <- paste0("Site_", 1:length(lon_input))
  colnames(std_site) <- c("0-5cm",
                          "5-15cm",
                          "15-30cm",
                          "30-60cm",
                          "60-100cm",
                          "100-200cm")
  #calculate organic carbon stock (ocs) as the sum of organic carbon density multiplied by layer thickness, the unit of ocs is kg/m2
  ocs_sum<-mean_site[,1]*(5-0)*0.01+mean_site[,2]*(15-5)*0.01+mean_site[,3]*(30-15)*0.01+mean_site[,4]*(60-30)*0.01+mean_site[,5]*(100-60)*0.01+mean_site[,6]*(200-100)*0.01 
  #calculate standard deviation of ocs as the square root of sum of variance of layer-specific  ocs, the unit of ocs is kg/m2, based on the paper https://www.sciencedirect.com/science/article/pii/S2215016122000462
  ocs_std<-sqrt((std_site[,1]*(5-0)*0.01)^2+(std_site[,2]*(15-5)*0.01)^2+(std_site[,3]*(30-15)*0.01)^2+(std_site[,4]*(60-30)*0.01)^2+(std_site[,5]*(100-60)*0.01)^2+(std_site[,6]*(200-100)*0.01)^2)
  return(list("Total OCS" = ocs_sum, "Standard deviation of OCS" = ocs_std))
}

