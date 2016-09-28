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
debias.met <- function(outfolder, source_met, train_met, site_id, de_method='mean', overwrite=FALSE, verbose=FALSE, ...){  
  require(PEcAn.utils)
  require(lubridate)
  require(ncdf4)
  outfolder = paste0(outfolder,"_site_",paste0(site_id %/% 1000000000, "-", site_id %% 1000000000))
  
  var = data.frame(DAP.name = c("tas","rlds","ps","rsds","uas","vas","huss","pr"),
                   CF.name = c("air_temperature","surface_downwelling_longwave_flux_in_air","air_pressure","surface_downwelling_shortwave_flux_in_air","eastward_wind","northward_wind","specific_humidity","precipitation_flux"),
                   units = c('Kelvin',"W/m2","Pascal","W/m2","m/s","m/s","g/g","kg/m2/s")
  )
  
  #Need to pull the year from the string filename, this assumes that the year is always placed right before '.nc'
  substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }
  sub_str= substrRight(source_met, 7)
  year = substr(sub_str,1, 4)
  #Read in the two datasets, with the dimensions of the source dataset being named sou.list
  sou = list()
  sou.list = list()
  tem = nc_open(source_met)
  dim = tem$dim
  for (j in 1:length(var$CF.name)){
    sou[[j]] = ncvar_get(tem,as.character(var$CF.name[j]))
    sou.list[[j]] = ncvar_def(name=as.character(var$CF.name[j]), units=as.character(var$units[j]), dim=dim, missval=-999, verbose=verbose)
  }
  lat_sou = as.numeric(ncvar_get(tem,"latitude"))
  lon_sou = as.numeric(ncvar_get(tem,"longitude"))
  year = as.numeric(year)
  nc_close(tem)
  
#Need to try and find the closest ameriflux site and year based on the lat/lon/year given 
#download function (lat_sou,lon_sou,year)
#   find closest fluxnet site and then download year, if not available, try next year up from 2006 to 2015? 
  train = list()
  tow = nc_open(train_met)
  for (j in 1:length(var$CF.name)){
    train[[j]] = ncvar_get(tow,as.character(var$CF.name[j]))
  }
  nc_close(tow)
  
  #Create dataframes from the lists of data pulled from the source/train and give them column names 
  sou = data.frame(sou)
  train = data.frame(train)
  colnames(sou) = c("air_temperature","surface_downwelling_longwave_flux_in_air","air_pressure","surface_downwelling_shortwave_flux_in_air","eastward_wind","northward_wind","specific_humidity","precipitation_flux")
  colnames(train) = c("air_temperature","surface_downwelling_longwave_flux_in_air","air_pressure","surface_downwelling_shortwave_flux_in_air","eastward_wind","northward_wind","specific_humidity","precipitation_flux")
  
  #Grab the means/medians of the source and train, find the difference, and correct the source dataset accordingly

  sou_add = data.frame(sou$air_temperature, sou$air_pressure, sou$eastward_wind, sou$northward_wind)
  sou_mult = data.frame(sou$surface_downwelling_longwave_flux_in_air, sou$surface_downwelling_shortwave_flux_in_air, sou$specific_humidity, sou$precipitation_flux)
  train_add = data.frame(train$air_temperature, train$air_pressure, train$eastward_wind, train$northward_wind)
  train_mult = data.frame(train$surface_downwelling_longwave_flux_in_air, train$surface_downwelling_shortwave_flux_in_air, train$specific_humidity, train$precipitation_flux)
  add_var = c("air_temperature", "air_pressure", "eastward_wind", "northward_wind")
  mult_var = c("surface_downwelling_longwave_flux_in_air","surface_downwelling_shortwave_flux_in_air","specific_humidity","precipitation_flux")
    
  if (de_method == 'mean'){
    mean_sou_add = apply(sou_add,2,mean)
    mean_train_add = apply(train_add,2,mean)
    mean_diff = mean_train - mean_sou
    mean_ratio = mean_train/mean_sou
    debi_add = list()
    debi_mult = list()
    for (k in 1:length(add_var)){
      debi_add[[k]] = (sou_add[[k]] + mean_diff[[k]])
    }
    for (k in 1:length(mult_var)){
      debi_mult[[k]] = (sou_mult[[k]]*mean_ratio[[k]])
    }
    debi_add = data.frame(debi_add)
    colnames(debi_add) = add_var
    debi_mult = data.frame(debi_mult)
    colnames(debi_mult) = mult_var
    debi = data.frame(debi_add,debi_mult)
  } else {
    if(de_method == 'median'){
      med_sou_add = apply(sou_add,2,median)
      med_train_add = apply(train_add,2,median)
      med_diff = med_train - med_sou
      med_ratio = med_train/med_sou
      debi_add = list()
      debi_mult = list()
      for (k in 1:length(add_var)){
        debi_add[[k]] = (sou_add[[k]] + med_diff[[k]])
      }
      for (k in 1:length(mult_var)){
        debi_mult[[k]] = (sou_mult[[k]]*med_ratio[[k]])
      }
      debi_add = data.frame(debi_add)
      colnames(debi_add) = add_var
      debi_mult = data.frame(debi_mult)
      colnames(debi_mult) = mult_var
      debi = data.frame(debi_add,debi_mult)
      }
    }
  }
  
  debi$precipitation_flux[debi$precipitation_flux < 0] <- 0
  debi$specific_humidity[debi$specific_humidity < 0] <- 0


  rows = 1
  dir.create(outfolder, showWarnings=FALSE, recursive=TRUE)
  results <- data.frame(file=character(rows), host=character(rows),
                        mimetype=character(rows), formatname=character(rows),
                        startdate=character(rows), enddate=character(rows),
                        dbfile.name = paste("debias.met",sep="."),
                        stringsAsFactors = FALSE)
  
  loc.file = file.path(outfolder,paste("debias",year,"nc",sep="."))
  
  loc <- nc_create(filename=loc.file, vars=sou.list, verbose=verbose)
  for(j in 1:nrow(var)){
    ncvar_put(nc=loc, varid=as.character(var$CF.name[j]), vals=debi[[j]])
  }
  nc_close(loc)
  
  results$file <- loc.file
  results$host <- fqdn()
  results$startdate <- paste0(year,"-01-01 00:00:00")
  results$enddate <- paste0(year,"-12-31 23:59:59")
  results$mimetype <- 'application/x-netcdf'
  results$formatname <- 'CF Meteorology'
  
  invisible(results)
}


