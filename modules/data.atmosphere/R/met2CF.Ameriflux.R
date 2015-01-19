##' Get meteorology variables from Ameriflux L2 netCDF files and convert to netCDF CF format
##'
##' @name met2CF.Ameriflux
##' @title met2CF.Ameriflux
##' @export
##' @param in.path
##' @param in.prefix
##' @param outfolder
##' 
##' @author Josh Mantooth, Mike Dietze, Elizabeth Cowdery, Ankur Desai
met2CF.Ameriflux <- function(in.path, in.prefix, outfolder, start_date, end_date, overwrite=FALSE, verbose=FALSE){

  #---------------- Load libraries. -----------------------------------------------------------------#
#  require(PEcAn.all)
#  require(RPostgreSQL)
  require(ncdf4)
  require(PEcAn.utils)
  require(geonames)
  require(lubridate)
  #--------------------------------------------------------------------------------------------------#  

  # get start/end year code works on whole years only
  start_year <- year(start_date)
  end_year <- year(end_date)
  
  ## get file names
  files = dir(in.path,in.prefix)
  files = files[grep(pattern="*.nc",files)]
  
  if(length(files) == 0) {
    ## send warning
    
    return(NULL)
  }  
  
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
    nc1 <- nc_open(old.file,write=TRUE)
        
    # get dimension and site info
    tdim <- nc1$dim[["DTIME"]]
    
    # create new coordinate dimensions based on site location lat/lon
    latlon <- getLatLon(nc1)
    
    #Ameriflux L2 files are in "local time" - figure this out and add to time units attribute
    #Check if timezone is already in time units, if not, figure it out from lat/lon and add it in
    tdimunit <- unlist(strsplit(tdim$units," "))
    tdimtz <- substr(tdimunit[length(tdimunit)],1,1)
    if ((tdimtz=="+")||(tdimtz=="-")) {
      lst <- tdimunit[length(tdimunit)] #already in definition, leave it alone
    } else {
      options(geonamesUsername="carya") #login to geoname server
      lst <- GNtimezone(latlon[1], latlon[2], radius = 0)$gmtOffset 
      if (lst>=0) {lststr<-paste("+",lst,sep="") } else {lststr<-as.character(lst) }
      tdim$units <- paste(tdim$units,lststr,sep=" ")   
    }
    
    lat <- ncdim_def(name='latitude', units='', vals=1:1, create_dimvar=FALSE)
    lon <- ncdim_def(name='longitude', units='', vals=1:1, create_dimvar=FALSE)
    time <- ncdim_def(name='time', units=tdim$units, vals=tdim$vals, create_dimvar=TRUE, unlim=TRUE)
    dim=list(lat,lon,time)
    
    # copy lat attribute to latitude
    print(latlon)
    print(lst)
    var <- ncvar_def(name="latitude",
                     units="degree_north",
                     dim=(list(lat,lon,time)), missval=as.numeric(-9999))
    nc2 <- nc_create(filename=new.file, vars=var, verbose=verbose)
    
    ncvar_put(nc=nc2, varid='latitude', vals=rep(latlon[1], tdim$len))
    
    # copy lon attribute to longitude
    var <- ncvar_def(name="longitude",
                     units="degree_east",
                     dim=(list(lat,lon,time)), missval=as.numeric(-9999))
    nc2 <- ncvar_add(nc=nc2, v=var, verbose=verbose)
    ncvar_put(nc=nc2, varid='longitude', vals=rep(latlon[2], tdim$len))
    
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
    
    # convert CO2 to mole_fraction_of_carbon_dioxide_in_air
    copyvals(nc1=nc1, var1='CO2',
             nc2=nc2, var2='mole_fraction_of_carbon_dioxide_in_air', units2='mole/mole', dim2=dim, 
             conv=function(x) { x * 1e6 }, verbose=verbose)
    
    # convert TS1 to soil_temperature
    copyvals(nc1=nc1, var1='TS1',
             nc2=nc2, var2='soil_temperature', units2='degrees K', dim2=dim, 
             conv=function(x) { x + 273.15 }, verbose=verbose)

    # copy RH to relative_humidity
    copyvals(nc1=nc1, var1='RH',
             nc2=nc2, var2='relative_humidity', dim2=dim, 
             verbose=verbose)

    # convert RH to SH
    rh <- ncvar_get(nc=nc,varid='RH')
    rh.sub <- which(rh > -6999) #select non-missing data
    rh.sh <- as.vector(rh.sub/100) #percent to proportion: needed for conversion
    rh.sh <- replace(x=rh,list=rh.sub,values=rh.sh) #insert proportion values into RH vector
    ta.rh <- ta[rh.sub] # use T coincident with RH
    sh.miss <- rh2qair(rh=rh.sh[rh.sub],T=ta.rh) #conversion, doesn't include missvals. was rh2rv
    sh <- replace(x=rh,list=rh.sub,values=sh.miss) #insert Kelvin values into vector
    sh.var <- ncvar_def(name='specific_humidity',units='kg/kg',dim=list(tdim),missval= -9999) #define netCDF variable, doesn't include longname and comments
    nc = ncvar_add(nc=nc,v=sh.var,verbose=TRUE) #add variable to existing netCDF file
    ncvar_put(nc,varid='specific_humidity',vals=sh)
    ncatt_put(nc=nc,varid='RH',attname='units',attval='percent') 
    nc <- ncvar_rename(nc=nc,'RH','relative_humidity')
    
    # fixing APARpct
    ncatt_put(nc=nc,varid='APARpct',attname='units',attval='percent') 
    # fixing ZL
    ncatt_put(nc=nc,varid='ZL',attname='units',attval='m/m') 

    nc_close(nc)
  
  }  ## end loop over files
 
 
}   ## end netCDF CF conversion ##

