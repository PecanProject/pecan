#
##' @title soilgrids.soc_extract
##' @name  soilgrids.soc_extract
##' 
##' @importFrom terra rast vect project
##' @importFrom reshape2 melt
##' 
##' @export
##' @author Qianyu Li, Shawn P. Serbin
##' 
soilgrids.soc_extract <- function (lon_input, lat_input) {
  i <- 1
  socquant <- matrix(NA, nrow = 6, ncol = length(lon_input) * 5)
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
    
    #Info about uncertainty in https://www.isric.org/explore/soilgrids/faq-soilgrids. The uncertainty layer is the ratio between the inter-quantile range (90% prediction interval width) and the median : (Q0.95-Q0.05)/Q0.50.
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
    
    
    #extract the mean soc values
    lonlat <- cbind(lon_input, lat_input)
    p <-
      terra::vect(lonlat, crs = "+proj=longlat +datum=WGS84") # Users need to provide lon/lat
    newcrs <-
      "+proj=igh +datum=WGS84 +no_defs +towgs84=0,0,0" #Soilgrids data is using Homolosine projection https://www.isric.org/explore/soilgrids/faq-soilgrids
    p_rob <-
      terra::project(p, newcrs) # We need to transform the point vector to data with Homolosine projection
    soc_mean <-
      terra::extract(soc_mean_ras, p_rob) #the original unit is dg/kg
    soc_mean_real <-
      soc_mean[, 2] / 10 #change the unit to more common g/kg
    
    #calculate uncertainty:
    soc_Q0.05_map <- terra::extract(soc_Q0.05_ras, p_rob)
    soc_Q0.50_map <- terra::extract(soc_Q0.50_ras, p_rob)
    soc_Q0.95_map <- terra::extract(soc_Q0.95_ras, p_rob)
    soc_Q0.05_real <- soc_Q0.05_map[, 2] / 10
    soc_Q0.50_real <- soc_Q0.50_map[, 2] / 10
    soc_Q0.95_real <- soc_Q0.95_map[, 2] / 10
    soc_uncer <-
      (soc_Q0.95_real - soc_Q0.05_real) / soc_Q0.50_real #uncertainty provided by the datasets
    
    socquant[i,] <-
      c(soc_mean_real,
        soc_Q0.05_real,
        soc_Q0.50_real,
        soc_Q0.95_real,
        soc_uncer)
    rownames(socquant) <-
      c("0-5", "5-15", "15-30", "30-60", "60-100", "100-200")
    colnames(socquant) <-
      c(
        rep("Mean", length(lon_input)),
        rep("0.05", length(lon_input)),
        rep("0.5", length(lon_input)),
        rep("0.95", length(lon_input)),
        rep("Uncertainty", length(lon_input))
      )
    i <- i + 1
  }
  soc_fit <- melt(socquant[, 1:4], id.vars = c("Mean"))
  colnames(soc_fit) <- c("Depth", "Quantile", "Value")
  soc_fit$Variable <- rep("soc", length(nrow(soc_fit)))
  dat <- split(soc_fit, soc_fit$Depth)
  
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
  mean <- bestPar[1, ] / bestPar[2, ]
  std <- sqrt(bestPar[1, ] / bestPar[2, ] ^ 2)
  return(list("Mean" = mean, "Standard dev" = std))
}