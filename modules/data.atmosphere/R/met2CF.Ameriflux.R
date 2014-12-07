# helper function to copy variables and attributes from one
# nc file to another. This will do conversion of the variables
# as well as on the min/max values
copyvals <- function(nc1, var1, nc2, var2, dim2, units2=NA, conv=NULL, missval=-6999.0, verbose=FALSE) {
    vals <- ncvar_get(nc=nc1, varid=var1)
    vals[vals==-6999 || vals==-9999] <- NA
    if (!is.null(conv)) {
      vals <- lapply(vals, conv)
    }
    if (is.na(units2)) {
      units2 <- ncatt_get(nc=nc1, varid=var1, attname='units', verbose=verbose)$value
    }
    var <- ncvar_def(name=var2, units=units2, dim=dim2, missval=missval, verbose=verbose)
    nc2 <- ncvar_add(nc=nc2, v=var, verbose=verbose)
    ncvar_put(nc=nc2, varid=var2, vals=vals)
    
    # copy and convert attributes
    att <- ncatt_get(nc1, var1, 'long_name')
    if (att$hasatt) {
      val <- att$value
      ncatt_put(nc=nc2, varid=var2, attname='long_name', attval=val)
    }
    
    att <- ncatt_get(nc1, var1, 'valid_min')
    if (att$hasatt) {
      val <- ifelse(is.null(conv), att$value, conv(att$value))
      ncatt_put(nc=nc2, varid=var2, attname='valid_min', attval=val)
    }
    
    att <- ncatt_get(nc1, var1, 'valid_max')
    if (att$hasatt) {
      val <- ifelse(is.null(conv), att$value, conv(att$value))
      ncatt_put(nc=nc2, varid=var2, attname='valid_max', attval=val)
    }
    
    att <- ncatt_get(nc1, var1, 'comment')
    if (att$hasatt) {
      val <- sub(', -9999.* = missing value, -6999.* = unreported value', '', att$value)
      ncatt_put(nc=nc2, varid=var2, attname='comment', attval=val)
    }
}

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
met2CF.Ameriflux <- function(start_year, end_year, in.path, in.prefix, outfolder, overwrite=FALSE, verbose=FALSE){
  #---------------- Load libraries. -----------------------------------------------------------------#
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
    
    #open raw ameriflux
    nc <- nc_open(old.file,write=TRUE)
        
    # get dimension and site info
    tdim <- nc1$dim[["DTIME"]]
    loc <- ncatt_get(nc=nc1, varid=0, attname='site_location')
    
    # create new coordinate dimensions based on site location lat/lon
    lat <- ncdim_def(name='latitude', units='', vals=1:1, create_dimvar=FALSE)
    lon <- ncdim_def(name='longitude',units='',vals=1:1, create_dimvar=FALSE)
    time <- ncdim_def(name='time', units='', vals=tdim$vals, create_dimvar=TRUE, unlim=TRUE)
    dim=list(lat,lon,time)
    
    # copy lat attribute to latitude
    vals <- rep(as.numeric(substr(loc$value,20,28)), tdim$len)
    var <- ncvar_def(name="latitude",
                     units="degree_north",
                     dim=(list(lat,lon,time)), missval=as.numeric(-9999))
    nc2 <- nc_create(filename=new.file, vars=var, verbose=verbose)
    ncvar_put(nc=nc2, varid='latitude', vals=vals)

    # copy lon attribute to longitude
    vals <- rep(as.numeric(substr(loc$value,40,48)), tdim$len)
    var <- ncvar_def(name="longitude",
                     units="degree_east",
                     dim=(list(lat,lon,time)), missval=as.numeric(-9999))
    nc2 <- ncvar_add(nc=nc2, v=var, verbose=verbose)
    ncvar_put(nc=nc2, varid='longitude', vals=vals)
    
    # Convert all variables, this will include conversions or computations
    # to create values from original file. In case of conversions the steps
    # will pretty much always be:
    #  a) get values from original file
    #  b) set -6999 and -9999 to NA
    #  c) do unit conversions
    #  d) create output variable
    #  e) write results to new file

    # convert TA to air_temperature
    copyvals(nc1=nc1, var1='TA',
             nc2=nc2, var2='air_temperature', units2='degrees K', dim2=dim, 
             conv=function(x) { x + 273.15 }, verbose=verbose)
    
    # convert PRESS to air_pressure
    copyvals(nc1=nc1, var1='PRESS',
             nc2=nc2, var2='air_pressure', units2='Pa', dim2=dim, 
             conv=function(x) { x * 1000 }, verbose=verbose)
    
    # convert TS1 to soil_temperature
    copyvals(nc1=nc1, var1='TS1',
             nc2=nc2, var2='soil_temperature', units2='degrees K', dim2=dim, 
             conv=function(x) { x + 273.15 }, verbose=verbose)

    # copy RH to relative_humidity
    copyvals(nc1=nc1, var1='RH',
             nc2=nc2, var2='relative_humidity', dim2=dim, 
             verbose=verbose)

    # convert RH to SH
    rh <- ncvar_get(nc=nc1, varid="RH")
    rh[rh==-6999 || rh==-9999] <- NA
    rh <- rh / 100
    ta <- ncvar_get(nc=nc1, varid="TA")
    ta[ta==-6999 || ta==-9999] <- NA
    ta <- ta + 273.15
    sh <- rh2qair(rh=rh,T=ta)
    var <- ncvar_def(name='specific_humidity', units='kg/kg', dim=dim, missval=-6999.0, verbose=verbose)
    nc2 <- ncvar_add(nc=nc2, v=var, verbose=verbose)
    ncvar_put(nc=nc2, varid='specific_humidity', vals=sh)
    
    # convert VPD to water_vapor_saturation_deficit
    # HACK : conversion will make all values < 0 to be NA
    copyvals(nc1=nc1, var1='VPD',
             nc2=nc2, var2='water_vapor_saturation_deficit', units2='mol m-2 s-1', dim2=dim, 
             conv=function(x) { ifelse(x<0, NA, x*1000) }, verbose=verbose)

    # copy Rg to surface_downwelling_shortwave_flux_in_air
    copyvals(nc1=nc1, var1='Rg',
             nc2=nc2, var2='surface_downwelling_shortwave_flux_in_air', dim2=dim, 
             verbose=verbose)

    # copy Rgl to surface_downwelling_longwave_flux_in_air
    copyvals(nc1=nc1, var1='Rgl',
             nc2=nc2, var2='surface_downwelling_longwave_flux_in_air', dim2=dim, 
             verbose=verbose)

    # convert PAR to surface_downwelling_photosynthetic_photon_flux_in_air
    copyvals(nc1=nc1, var1='PAR',
             nc2=nc2, var2='surface_downwelling_photosynthetic_photon_flux_in_air', units2='mol m-2 s-1', dim2=dim, 
             conv=function(x) { x / 1e6 }, verbose=verbose)

    # copy WD to wind_direction (not official CF)
    copyvals(nc1=nc1, var1='WD',
             nc2=nc2, var2='wind_direction', dim2=dim, 
             verbose=verbose)

    # copy WS to wind_speed
    copyvals(nc1=nc1, var1='WS',
             nc2=nc2, var2='wind_speed', dim2=dim, 
             verbose=verbose)

    # convert PREC to precipitation_flux
    t <- tdim$vals
    min <- 0.02083/30 # 0.02083 time = 30 minutes
    timestep <- round(x=mean(diff(t))/min,digits=1) # round to nearest 0.1 minute
    copyvals(nc1=nc1, var1='PREC',
             nc2=nc2, var2='precipitation_flux', units2='kg/m^2/s', dim2=dim, 
             conv=function(x) { x/timestep/60 }, verbose=verbose)

    # convert wind speed and wind direction to eastward_wind and northward_wind
    wd <- ncvar_get(nc=nc1, varid='WD') #wind direction
    wd[wd==-6999 || wd==-9999] <- NA
    ws <- ncvar_get(nc=nc1, varid='WS') #wind speed
    ws[ws==-6999 || ws==-9999] <- NA
    ew <- ws * cos(wd * (pi/180))
    nw <- ws * sin(wd * (pi/180))
    max <- ncatt_get(nc=nc1, varid='WS', 'valid_max')$value

    var <- ncvar_def(name='eastward_wind', units='m/s', dim=dim, missval=-6999.0, verbose=verbose)
    nc2 <- ncvar_add(nc=nc2, v=var, verbose=verbose)
    ncvar_put(nc=nc2, varid='eastward_wind', vals=ew)
    ncatt_put(nc=nc2, varid='eastward_wind', attname='valid_min', attval=-max)
    ncatt_put(nc=nc2, varid='eastward_wind', attname='valid_max', attval=max)

    var <- ncvar_def(name='northward_wind', units='m/s', dim=dim, missval=-6999.0, verbose=verbose)
    nc2 <- ncvar_add(nc=nc2, v=var, verbose=verbose)
    ncvar_put(nc=nc2, varid='northward_wind', vals=nw)
    ncatt_put(nc=nc2, varid='northward_wind', attname='valid_min', attval=-max)
    ncatt_put(nc=nc2, varid='northward_wind', attname='valid_max', attval=max)

    # add global attributes from original file
    cp.global.atts <- ncatt_get(nc=nc1, varid=0)
    for(j in 1:length(cp.global.atts)){
      ncatt_put(nc=nc2, varid=0, attname=names(cp.global.atts)[j], attval=cp.global.atts[[j]])
    }

    # done, close both files
    nc_close(nc)
    nc_close(nc2)
  }  ## end loop over years
  
  invisible(results)
}   ## end netCDF CF conversion ##

