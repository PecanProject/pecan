##' debias.met takes source_met, train_met and output new_met that debiases source_met against train_met
##' @name debias.met
##' @title debias.met
##' @export
##' @param outfolder
##' @param source_met - the met data that you want to be altered 
##' @param train_met - the met data that we use to train the other dataset
##' @param de_method - select which debias method you would like to use, options are 'normal', 'linear regression'
##' @param site.id
##' @author James Simkins
debias.met <- function(outfolder, source_met, train_met, site_id, de_method='linear',
                       overwrite=FALSE, verbose=FALSE, ...){  
  library(PEcAn.utils)
  library(lubridate)
  library(ncdf4)
  outfolder = paste0(outfolder,"_site_",paste0(site_id %/% 1000000000, "-", site_id %% 1000000000))
  
  var = data.frame(CF.name = c("air_temperature","surface_downwelling_longwave_flux_in_air","air_pressure",
                               "surface_downwelling_shortwave_flux_in_air","eastward_wind","northward_wind",
                               "specific_humidity","precipitation_flux"),
                   units = c('Kelvin',"W/m2","Pascal","W/m2","m/s","m/s","g/g","kg/m2/s")
  )

  #Need to pull the year from the string filename, this assumes that the year is always placed right before '.nc'
  substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }
  sub_str <- substrRight(source_met, 7)
  year <- substr(sub_str,1, 4)
  
  
  #Read in the two datasets, with the dimensions of the source dataset being named sou.list
  sou <- list()
  sou.list <- list()
  tem <- nc_open(source_met)
  dim <- tem$dim
  for (j in 1:length(var$CF.name)){
    sou[[j]] <- ncvar_get(tem,as.character(var$CF.name[j]))
    sou.list[[j]] <- ncvar_def(name=as.character(var$CF.name[j]), 
                               units=as.character(var$units[j]), 
                               dim=dim, 
                               missval=-999, 
                               verbose=verbose)
  }
  lat_sou <- as.numeric(ncvar_get(tem,"latitude"))
  lon_sou <- as.numeric(ncvar_get(tem,"longitude"))
  year <- as.numeric(year)
  nc_close(tem)
  
# Load in the 'better' data that will be used to train the source. Most of the time this will be observed data. 
  train <- list()
  tow <- nc_open(train_met)
  for (j in 1:length(var$CF.name)){
    train[[j]] <- ncvar_get(tow,as.character(var$CF.name[j]))
  }
  nc_close(tow)
  
  #Create dataframes from the lists of data pulled from the source/train and give them column names 
  sou <- data.frame(sou)
  train <- data.frame(train)
  colnames(sou) <- var$CF.name
  colnames(train) <- var$CF.name
  #Grab the means/medians of the source and train, find the difference, and correct the source dataset accordingly
  #The following separate variables based on properties so we can appropriately debias based on means/medians
  sou_add <- data.frame(sou$air_temperature, sou$air_pressure, sou$eastward_wind, sou$northward_wind)
  sou_mult <- data.frame(sou$surface_downwelling_longwave_flux_in_air, sou$surface_downwelling_shortwave_flux_in_air, sou$specific_humidity, sou$precipitation_flux)
  train_add <- data.frame(train$air_temperature, train$air_pressure, train$eastward_wind, train$northward_wind)
  train_mult <- data.frame(train$surface_downwelling_longwave_flux_in_air, train$surface_downwelling_shortwave_flux_in_air, train$specific_humidity, train$precipitation_flux)
  add_var <- c("air_temperature", "air_pressure", "eastward_wind", "northward_wind")
  mult_var <- c("surface_downwelling_longwave_flux_in_air","surface_downwelling_shortwave_flux_in_air","specific_humidity","precipitation_flux")
  # These are for the linear regression argument, the for loop upscales the training dataset to match the length of the source dataset because they must be equal lengths
  reso <- nrow(sou)
  step <- floor(nrow(train) / nrow(sou))
  lin_train <- data.frame()
  for (n in 1:length(var$CF.name)){
    for (x in 1:reso){
      lin_train[x,n] <- mean(train[(x * step-step + 1):(x * step),n])}
  }
  colnames(lin_train) <- var$CF.name
  
  ### De_method routines!!!!! ###
  for (n in 1:length(var$DAP.name)){
    if (de_method == 'mean'){
      mean_sou <- apply(sou,2,mean)
      mean_train <- apply(train,2,mean)
      mean_diff <- mean_train - mean_sou
      mean_ratio <- mean_train / mean_sou
      debi_add <- list()
      debi_mult <- list()
    for (k in 1:length(add_var)){
      debi_add[[k]] <- (sou_add[[k]] + mean_diff[[k]])
    }
    for (k in 1:length(mult_var)){
      debi_mult[[k]] <- (sou_mult[[k]] * mean_ratio[[k]])
    }
    debi_add <- data.frame(debi_add)
    colnames(debi_add) <- add_var
    debi_mult <- data.frame(debi_mult)
    colnames(debi_mult) <- mult_var
    debi <- data.frame(debi_add,debi_mult)
  } else {
    if(de_method == 'median'){
      med_sou <- apply(sou_add,2,median)
      med_train<- apply(train_add,2,median)
      med_diff <- med_train - med_sou
      med_ratio <- med_train / med_sou
      debi_add <- list()
      debi_mult <- list()
      for (k in 1:length(add_var)){
        debi_add[[k]] <- (sou_add[[k]] + med_diff[[k]])
      }
      for (k in 1:length(mult_var)){
        debi_mult[[k]] <- (sou_mult[[k]] * med_ratio[[k]])
      }
      debi_add <- data.frame(debi_add)
      colnames(debi_add) <- add_var
      debi_mult <- data.frame(debi_mult)
      colnames(debi_mult) <- mult_var
      debi <- data.frame(debi_add,debi_mult)
      } else {
      if(de_method == 'linear'){
        debi <- list()
        for(i in 1:length(var$CF.name)){
          if(all(is.na(sou[[i]]))== FALSE & all(is.na(lin_train[[i]])) == FALSE){
            lin <- lm(lin_train[[i]] ~ sou[[i]])
            x <- as.numeric(lin$coefficients[2])
            b <- as.numeric(lin$coefficients[1])
            m <- sou[[i]]
          if (var$CF.name[[i]] == 'precipitation_flux'){b = 0}
            if (var$CF.name[[i]] == 'specific_humidity'){b = 0}
          debi[[i]] <- (m * x + b)}
          else {
            if(all(is.na(sou[[i]])) == TRUE | all(is.na(lin_train[[i]])) == TRUE){
              debi[[i]] <- NA}}}
        debi <- data.frame(debi)
        colnames(debi) <- var$CF.name}
      }
  }
  }

  # This step just ensures that we aren't breaking laws of nature by having negative precipitation or negative specific humidity 
  debi$precipitation_flux[debi$precipitation_flux < 0] <- 0
  debi$specific_humidity[debi$specific_humidity < 0] <- 0


  rows <- 1
  dir.create(outfolder, showWarnings=FALSE, recursive=TRUE)
  results <- data.frame(file=character(rows), host=character(rows),
                        mimetype=character(rows), formatname=character(rows),
                        startdate=character(rows), enddate=character(rows),
                        dbfile.name = paste("debias.met",sep="."),
                        stringsAsFactors = FALSE)
  
  loc.file = file.path(outfolder,paste("debias",year,"nc",sep="."))
  
  loc <- nc_create(filename=loc.file, vars=sou.list, verbose=verbose)
  for(j in seq_along(var$CF.name)){
    ncvar_put(nc=loc, varid=as.character(var$CF.name[j]), vals=debi[[j]])
  }
  nc_close(loc)
  
  results$file <- loc.file
  results$host <- fqdn()
  results$startdate <- paste0(year,"-01-01 00:00:00", tz = "UTC")
  results$enddate <- paste0(year,"-12-31 23:59:59", tz = "UTC")
  results$mimetype <- 'application/x-netcdf'
  results$formatname <- 'CF Meteorology'
  
  invisible(results)
}

#debias.met('debi','GFDL.CM3.rcp45.r1i1p1.2006.nc', 'US-WCr.2006.nc', 4, de_method = 'linear')
