rm(list = ls())
library(terra)
library(reshape2)

soc_extract <- function (lon_input, lat_input) {
  i <- 1
  #create a variable to store mean and quantile of SOC for each soil depth
  socquant <- matrix(NA, nrow = 6, ncol = length(lon_input) * 4)
  #create a variable for site ID
  siteid <- matrix(NA, nrow = 6, ncol = length(lon_input) * 4)
  lonlat <- cbind(lon_input, lat_input)
  for (dep in c("0-5cm",
                "5-15cm",
                "15-30cm",
                "30-60cm",
                "60-100cm",
                "100-200cm")) {
    soc_mean.url <-
      paste0(
        "/vsicurl?max_retry=30&retry_delay=60&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/soc/soc_",
        dep,
        "_mean.vrt"
      )
    
    soc_Q0.05.url <-
      paste0(
        "/vsicurl?max_retry=30&retry_delay=60&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/soc/soc_",
        dep,
        "_Q0.05.vrt"
      )
    
    soc_Q0.50.url <-
      paste0(
        "/vsicurl?max_retry=30&retry_delay=60&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/soc/soc_",
        dep,
        "_Q0.5.vrt"
      )
    
    soc_Q0.95.url <-
      paste0(
        "/vsicurl?max_retry=30&retry_delay=60&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/soc/soc_",
        dep,
        "_Q0.95.vrt"
      )
    
    soc_mean_ras <- terra::rast(soc_mean.url)
    soc_Q0.05_ras <- terra::rast(soc_Q0.05.url)
    soc_Q0.50_ras <- terra::rast(soc_Q0.50.url)
    soc_Q0.95_ras <- terra::rast(soc_Q0.95.url)
    
    p <-
      terra::vect(lonlat, crs = "+proj=longlat +datum=WGS84") # Users need to provide lon/lat
    newcrs <-
      "+proj=igh +datum=WGS84 +no_defs +towgs84=0,0,0" #Soilgrids data is using Homolosine projection https://www.isric.org/explore/soilgrids/faq-soilgrids
    p_rob <-
      terra::project(p, newcrs) # Transform the point vector to data with Homolosine projection
    
    #extract SOC values
    soc_mean <- terra::extract(soc_mean_ras, p_rob) #the original unit is dg/kg
    soc_Q0.05_map <- terra::extract(soc_Q0.05_ras, p_rob)
    soc_Q0.50_map <- terra::extract(soc_Q0.50_ras, p_rob)
    soc_Q0.95_map <- terra::extract(soc_Q0.95_ras, p_rob)
    
    #change the unit to more common g/kg
    soc_mean_real <- soc_mean[, 2] / 10
    soc_Q0.05_real <- soc_Q0.05_map[, 2] / 10
    soc_Q0.50_real <- soc_Q0.50_map[, 2] / 10
    soc_Q0.95_real <- soc_Q0.95_map[, 2] / 10
    
    socquant[i, ] <-
      c(soc_mean_real,
        soc_Q0.05_real,
        soc_Q0.50_real,
        soc_Q0.95_real)
    
    siteid [i, ] <- rep(1:length(lon_input), 4)
    rownames(socquant) <-
      c("0-5", "5-15", "15-30", "30-60", "60-100", "100-200")
    colnames(socquant) <-
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
  
  soc_fit <- melt(socquant, id.vars = c("Mean"))
  id_fit <- melt(siteid, id.vars = c("Mean"))
  colnames(soc_fit) <- c("Depth", "Quantile", "Value")
  soc_fit$Variable <- rep("soc", length(nrow(soc_fit)))
  soc_fit$siteid <- id_fit$value
  dat <- split(soc_fit, list(soc_fit$siteid, soc_fit$Depth))
  
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
  
  score <- lapply(dat, fitQ)
  bestPar <- sapply(score, function(f) {
    f$par
  })
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
  return(list("Mean" = mean_site, "Standard deviation" = std_site))
}

