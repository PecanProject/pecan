##' Get meteorology variables from Ameriflux L2 netCDF files and convert to netCDF CF format
##'
##' @name met2CF.Ameriflux
##' @title met2CF.Ameriflux
##' @export
##' @param start_year first year to be converted
##' @param end_year last year to be converted
##' @param in.path location on disk where inputs are stored
##' @param in.prefix prefix of input and output files
##' @param outfolder location on disk where outputs will be stored
##' @param overwrite should existing files be overwritten
##' 
##' @author Josh Mantooth, Mike Dietze, Elizabeth Cowdery
met2CF.Ameriflux <- function(start_year, end_year, in.path, in.prefix, outfolder, overwrite=FALSE){
  #---------------- Load libraries. -----------------------------------------------------------------#
#  require(PEcAn.all)
#  require(RPostgreSQL)
  require(ncdf4)
  #--------------------------------------------------------------------------------------------------#  
  
  if(!file.exists(outfolder)){
    dir.create(outfolder)
  }
  
  rows <- end_year - start_year + 1
  results <- data.frame(file=character(rows), host=character(rows),
                        mimetype=character(rows), formatname=character(rows),
                        startdate=character(rows), enddate=character(rows),
                        stringsAsFactors = FALSE)
  for(year in start_year:end_year){
    old.file <- file.path(in.path, paste(in.prefix, year, "nc", sep="."))
    new.file <- file.path(outfolder, paste(in.prefix, year, "nc", sep="."))
    
    # create array with results
    row <- year - start_year + 1
    results$file[row] <- new.file
    results$host[row] <- fqdn()
    results$startdate[row] <- paste0(year,"-01-01 00:00:00")
    results$enddate[row] <- paste0(year,"-12-31 23:59:59")
    results$mimetype[row] <- 'application/x-netcdf'
    results$formatname[row] <- 'CF'
    
    if (file.exists(new.file) && !overwrite) {
      logger.debug("File '", new.file, "' already exists, skipping to next file.")
      next
    }
    
    ## copy old file to new directory
    file.copy(old.file, new.file, overwrite=TRUE)
    
    ### rename time dimension
    system2("ncrename",paste("-d DTIME,time -v DTIME,time", new.file))
    
    ### if reading ameriflux .nc file ###
    nc <- nc_open(new.file,write=TRUE)
    
    #time dimension for adding new variables
    tdim = nc$dim[["time"]]
    
    # convert TA to air_temperature
    ta <- ncvar_get(nc, varid="TA")
    ta[ta==-6999 || ta==-9999] <- NA
    # unit conversion
    ta <- ta + 273.15
    ncatt_put(nc=nc,varid='TA',attname='units',attval='degrees K') 
    #ncvar_change_missval(nc, 'TA', -9999)
    ncvar_put(nc,varid='TA',vals=ta)
    nc <- ncvar_rename(nc=nc,'TA','air_temperature')
    
    # convert TS1 to soil_temperature
    # TODO: leave TS1 for now
    ts <- ncvar_get(nc=nc,varid='TS1')
    ts[ts==-6999 || ts==-9999] <- NA
    # unit conversion
    ts <- ts + 273.15
    ts.var <- ncvar_def(name='soil_temperature',units='degrees K',dim=list(tdim),missval=-9999) #define netCDF variable, doesn't include longname and comments
    nc <- ncvar_add(nc,v=ts.var,verbose=TRUE) #add variable to existing netCDF file
    ncvar_put(nc,varid='soil_temperature',vals=ts)
        
    #convert wind speed and wind direction to U and V
    wd <- ncvar_get(nc=nc,varid='WD') #wind direction
    ws <- ncvar_get(nc=nc,varid='WS') #wind speed
    sub <- which(ws > -6999 & wd > -6999)
    w.miss <- pmin(ws[-sub],wd[-sub])
    wd.sub <- wd[sub] #use wind direction coincident with windspeed
    u = v = rep(NA,length(ws))
    u[sub] <- ws[sub]*cos(wd.sub*(pi/180))
    v[sub] <- ws[sub]*sin(wd.sub*(pi/180))
    u[-sub] = w.miss
    v[-sub] = w.miss
    nc <- ncvar_rename(nc=nc,'WS','wind_speed') #CF name
    nc <- ncvar_rename(nc=nc,'WD','wind_direction') #CF name
    
    #create u and v variables and insert into file
    tdim = nc$dim[["time"]]
    u.var <- ncvar_def(name='eastward_wind',units='m/s',dim=list(tdim),missval= -9999) #define netCDF variable, doesn't include longname and comments
    nc <- ncvar_add(nc=nc,v=u.var,verbose=TRUE) #add variable to existing netCDF file
    ncvar_put(nc,varid='eastward_wind',vals=u)
    
    v.var <- ncvar_def(name='northward_wind',units='m/s',dim=list(tdim),missval= -9999) #define netCDF variable, doesn't include longname and comments
    nc <- ncvar_add(nc=nc,v=v.var,verbose=TRUE) #add variable to existing netCDF file
    ncvar_put(nc,varid='northward_wind',vals=v)
   
    # convert PRESS to air_pressure
    press <- ncvar_get(nc=nc,varid="PRESS")
    press[press==-6999 || press==-9999] <- NA
    # unit conversion
    press <- press * 1000
    ncatt_put(nc=nc,varid='PRESS',attname='units',attval='Pa') 
    #ncvar_change_missval(nc, 'PRESS', -9999)
    ncvar_put(nc,varid='PRESS',vals=press)
    nc <- ncvar_rename(nc,'PRESS', 'air_pressure', verbose=TRUE)
    
    # convert PAR to surface_downwelling_photosynthetic_photon_flux_in_air
    par <- ncvar_get(nc, varid="PAR")
    par[par==-6999 || par==-9999] <- NA
    # unit conversion
    par <- par / 1e6
    ncatt_put(nc=nc,varid='PAR',attname='units',attval='mol m-2 s-1') 
    #ncvar_change_missval(nc, 'PAR', -9999)
    ncvar_put(nc,varid='PAR',vals=par)
    nc <- ncvar_rename(nc,'PAR','surface_downwelling_photosynthetic_photon_flux_in_air', verbose=TRUE)
    
    # convert VPD to water_vapor_saturation_deficit
    vpd <- ncvar_get(nc, varid="VPD")
    vpd[vpd==-6999 || vpd==-9999] <- NA
    # HACK This needs fixing
    vpd[vpd<0] <- NA
    # unit conversion
    vpd <- vpd * 1000
    ncatt_put(nc=nc,varid='VPD',attname='units',attval='Pa') 
    #ncvar_change_missval(nc, 'VPD', -9999)
    ncvar_put(nc,varid='VPD',vals=vpd)
    nc <- ncvar_rename(nc,'VPD','water_vapor_saturation_deficit')
    
    # rename Rg to surface_downwelling_shortwave_flux
    nc <- ncvar_rename(nc=nc,'Rg','surface_downwelling_shortwave_flux')

    # rename Rgl to surface_downwelling_longwave_flux
    nc <- ncvar_rename(nc=nc,'Rgl','surface_downwelling_longwave_flux')
    
    #convert precipitation to CF standard
    time <- ncvar_get(nc=nc,varid="time")
    min <- 0.02083/30 #0.02083 time = 30 minutes
    timestep <- round(x=mean(diff(time))/min,digits=1) #round to nearest 0.1 minute
    prec <- ncvar_get(nc=nc,varid="PREC")
    prec.sub <- which(prec > -6999)
    prec.new <- prec[prec.sub]/timestep/60 #mm/s = kg/m2/s
    prec <- replace(x=prec,list=prec.sub,values=prec.new)
    ncatt_put(nc=nc,varid='PREC',attname='units',attval='kg/m^2/s') 
    ncvar_put(nc=nc, varid='PREC',vals=prec)
    nc <- ncvar_rename(nc=nc,'PREC','precipitation_flux')
    
    # convert RH to SH
    rh <- ncvar_get(nc=nc,varid='RH')
    rh.sub <- which(rh > -6999) #select non-missing data
    rh.sh <- as.vector(rh.sub/100) #percent to proportion: needed for conversion
    rh.sh <- replace(x=rh,list=rh.sub,values=rh.sh) #insert proportion values into RH vector
    ta.rh <- ta[rh.sub] # use T coincident with RH
    sh.miss <- rh2qair(rh=rh.sh[rh.sub],T=ta.rh) #conversion, doesn't include missvals. was rh2rv
    sh <- replace(x=rh,list=rh.sub,values=sh.miss) #insert Kelvin values into vector
    sh.var <- ncvar_def(name='specific_humidity',units='kg/kg',dim=list(tdim),missval= -9999) #define netCDF variable, doesn't include longname and comments
    nc <- ncvar_add(nc=nc,v=sh.var,verbose=TRUE) #add variable to existing netCDF file
    ncvar_put(nc,varid='specific_humidity',vals=sh)
    ncatt_put(nc=nc,varid='RH',attname='units',attval='percent') 
    nc <- ncvar_rename(nc=nc,'RH','relative_humidity')
    
    # fixing APARpct
    ncatt_put(nc=nc,varid='APARpct',attname='units',attval='percent') 
    # fixing ZL
    ncatt_put(nc=nc,varid='ZL',attname='units',attval='m/m') 
    
    #get site location attribute
    loc <- ncatt_get(nc=nc,varid=0,attname='site_location')
   # lat.value <- rep(as.numeric(substr(loc$value,20,28)),tdim$len)
   # lon.value <- rep(as.numeric(substr(loc$value,40,48)),tdim$len)
    lat.value <- as.numeric(substr(loc$value,20,28))
    lon.value <- as.numeric(substr(loc$value,40,48))
    
    #create new coordinate dimensions based on site location lat/lon
    lat <- ncdim_def(name='latitude',units='',vals=1:1,create_dimvar=FALSE)
    lon <- ncdim_def(name='longitude',units='',vals=1:1,create_dimvar=FALSE)
    
    #create site location variables
    lat.var <- ncvar_def(name='latitude',units='degree_north',dim=list(lat),missval=-9999) 
    nc <- ncvar_add(nc=nc,v=lat.var,verbose=TRUE) #add latitude to existing netCDF file
    ncvar_put(nc,varid='latitude',vals=lat.value)
    
    lon.var <- ncvar_def(name='longitude',units='degree_east',dim=list(lon),missval=-9999) 
    nc <- ncvar_add(nc=nc,v=lon.var,verbose=TRUE) #add longitude to existing netCDF file
    ncvar_put(nc,varid='longitude',vals=lon.value)
    
    nc_close(nc)
  }  ## end loop over files
  
  invisible(results)
}   ## end netCDF CF conversion ##

