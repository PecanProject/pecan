##' Download MACA CMIP5 outputs for a single grid point using OPeNDAP and convert to CF 
##' @name download.MACA
##' @title download.MACA
##' @export
##' @param outfolder
##' @param start_date , of the format "YEAR-01-01 00:00:00"
##' @param end_date , of the format "YEAR-12-31 23:59:59"
##' @param lat
##' @param lon
##' @param model , select which MACA model to run (options are BNU-ESM, CNRM-CM5, CSIRO-Mk3-6-0, bcc-csm1-1, bcc-csm1-1-m, CanESM2, GFDL-ESM2M, GFDL-ESM2G, HadGEM2-CC365, HadGEM2-ES365, inmcm4, MIROC5, MIROC-ESM, MIROC-ESM-CHEM, MRI-CGCM3, CCSM4, IPSL-CM5A-LR, IPSL-CM5A-MR, IPSL-CM5B-LR, NorESM1-M)
##' @param scenario , select which scenario to run (options are rcp45, rcp85)
##' @param ensemble_member , r1i1p1 is the only ensemble member available for this dataset, CCSM4 uses r6i1p1 instead
##'
##' @author James Simkins
download.MACA <- function(outfolder, start_date, end_date, site_id, lat.in, lon.in, model='IPSL-CM5A-LR', scenario='rcp85', ensemble_member='r1i1p1', 
                          overwrite=FALSE, verbose=FALSE, ...){  
  library(PEcAn.utils)
  library(lubridate)
  library(ncdf4)
  start_date <- as.POSIXlt(start_date, tz = "UTC")
  end_date <- as.POSIXlt(end_date, tz = "UTC")
  start_year <- year(start_date)
  end_year   <- year(end_date)
  site_id <- as.numeric(site_id)
  model <- paste0(model)
  scenario <- paste0(scenario)
  ensemble_member <- paste0(ensemble_member)
  outfolder <- paste0(outfolder,"_site_",paste0(site_id %/% 1000000000, "-", site_id %% 1000000000))
  
  if (model == 'CCSM4'){
    ensemble_member <- 'r6i1p1'
  }
  
  lat.in <- as.numeric(lat.in)
  lat <- lat.in - 25.063077926635742
  lat_MACA <- round(lat*24)
  lon.in <- as.numeric(lon.in)
  if (lon.in < 0){
    lon.in <- 180 - lon.in}
  lon <- lon.in - 235.22784423828125
  lon_MACA <- round(lon*24)
  

  dap_base <-'http://thredds.northwestknowledge.net:8080/thredds/dodsC/MACAV2'

  ylist <- seq(start_year,end_year,by=1)
  rows <- length(ylist)
  results <- data.frame(file=character(rows), host=character(rows),
                        mimetype=character(rows), formatname=character(rows),
                        startdate=character(rows), enddate=character(rows),
                        dbfile.name = paste("MACA",model,scenario,ensemble_member,sep="."),#"MACA",
                        stringsAsFactors = FALSE)
  
  
  var <- data.frame(DAP.name <- c("tasmax","tasmin","rsds","uas","vas","huss","pr","none","none","none"), 
                  long_DAP.name <- c("air_temperature","air_temperature","surface_downwelling_shortwave_flux_in_air",
                                    "eastward_wind","northward_wind","specific_humidity","precipitation","air_pressure", 
                                    "surface_downwelling_longwave_flux_in_air","air_temp"),
                  CF.name <- c("air_temperature_max","air_temperature_min","surface_downwelling_shortwave_flux_in_air",
                              "eastward_wind","northward_wind","specific_humidity","precipitation_flux","air_pressure", 
                              "surface_downwelling_longwave_flux_in_air","air_temperature"),
                  units <- c('Kelvin','Kelvin',"W/m2","m/s","m/s","g/g","kg/m2/s", "Pascal", "W/m2","Kelvin")
                      )
  
  
  for (i in 1:rows){
    year <- ylist[i]    
    ntime <- (1825)
    
    met_start <- 2006
    met_block <- 5
    url_year <- met_start + floor((year-met_start)/met_block)*met_block
    start_url <- paste0(url_year)
    end_url <- paste0(url_year+met_block-1)
    dir.create(outfolder, showWarnings=FALSE, recursive=TRUE)
    
    loc.file <- file.path(outfolder,paste("MACA",model,scenario,ensemble_member,year,"nc",sep="."))
    
    ## Create dimensions
    lat <- ncdim_def(name='latitude', units='degree_north', vals=lat.in, create_dimvar=TRUE)
    lon <- ncdim_def(name='longitude', units='degree_east', vals=lon.in, create_dimvar=TRUE)
    time <- ncdim_def(name='time', units="sec", vals=(1:365)*86400, create_dimvar=TRUE, unlim=TRUE)
    dim<-list(lat,lon,time)
    
    var.list <- list()
    dat.list <- list()
    
    ## get data off OpenDAP
    for(j in 1:length(var$CF.name)){
      dap_end <- paste0('/',
                        model,'/macav2metdata_',
                        var$DAP.name[j],'_',
                        model,'_',
                        ensemble_member,'_',
                        scenario,'_',
                        start_url,'_',
                        end_url,'_CONUS_daily.nc')
      dap_file <- paste0(dap_base,dap_end)
      if(j < 8){
      dap <- nc_open(dap_file)
      dat.list[[j]] <- ncvar_get(dap,as.character(var$long_DAP.name[j]),c(lon_MACA,lat_MACA,1),c(1,1,ntime))
      var.list[[j]] <- ncvar_def(name=as.character(var$CF.name[j]), units=as.character(var$units[j]), dim=dim, missval=-9999.0, verbose=verbose)
      nc_close(dap)
      } else {
        dat.list[[j]] <- NA
        var.list[[j]] <- ncvar_def(name=as.character(var$CF.name[j]), units=as.character(var$units[j]), dim=dim, missval=-9999.0, verbose=verbose)}
    }
    
    dat.list <- as.data.frame(dat.list)
    colnames(dat.list) <- c("air_temperature_max","air_temperature_min","surface_downwelling_shortwave_flux_in_air","eastward_wind","northward_wind","specific_humidity","precipitation_flux","air_pressure", "surface_downwelling_longwave_flux_in_air","air_temperature")
    #for (n in 1:1825){
     # dat.list[n,"air_pressure"] = 1
      #dat.list[n,"surface_downwelling_longwave_flux_in_air"] = 1}
    #take average of temperature min and max
    dat.list[["air_temperature"]] <- (dat.list[["air_temperature_max"]]+dat.list[["air_temperature_min"]])/2
    #convert mm precipitation to precipitation flux
    dat.list[["precipitation_flux"]] <- (dat.list[["precipitation_flux"]]/(24*3600))

    
    #read in a 5 year file, but only storing 1 year at a time, so this selects the particular year of the 5 year span that you want
    if (year%%5 == 1){
      dat.list = dat.list[1:365,]
    }
    if (year%%5 == 2){
      dat.list = dat.list[366:730,]
    }
    if (year%%5 == 3){
      dat.list = dat.list[731:1095,]
    }
    if (year%%5 == 4){
      dat.list = dat.list[1096:1460,]
    }
    if (year%%5 == 0){
      dat.list = dat.list[1461:1825,]
    }
    
    ## put data in new file
    loc <- nc_create(filename=loc.file, vars=var.list, verbose=verbose)
    for(j in seq_along(var$CF.name)){
      ncvar_put(nc=loc, varid=as.character(var$CF.name[j]), vals=dat.list[[j]])
    }
    nc_close(loc)
    
    results$file[i] <- loc.file
    results$host[i] <- fqdn()
    results$startdate[i] <- paste0(year,"-01-01 00:00:00")
    results$enddate[i] <- paste0(year,"-12-31 23:59:59")
    results$mimetype[i] <- 'application/x-netcdf'
    results$formatname[i] <- 'CF Meteorology'
    
  }
  
  invisible(results)
}

#download.MACA('maca','2006-01-01 00:00:00','2006-12-31 23:59:59',5,45,-90)

