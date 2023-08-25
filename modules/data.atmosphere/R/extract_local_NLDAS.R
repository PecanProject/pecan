##' Extract NLDAS from local download
##' Extract NLDAS meteorology for a point from a local download of the full grid
# -----------------------------------
# Description
# -----------------------------------
##' @author Christy Rollinson
##' @description This function extracts NLDAS data from grids that have been downloaded and stored locally.
##'              Once upon a time, you could query these files directly from the internet, but now they're 
##'              behind a tricky authentication wall. Files are saved as a netCDF file in CF conventions. 
##'              These files are ready to be used in the general PEcAn workflow or fed into the downscaling 
##'              workflow.
# ----------------------------------- 
# Parameters
# -----------------------------------
##' @param outfolder - directory where output files will be stored
##' @param in.path - path to the raw full grids
##' @param start_date - first day for which you want to extract met (yyyy-mm-dd)
##' @param end_date - last day for which you want to extract met (yyyy-mm-dd)
##' @param lat.in site latitude in decimal degrees
##' @param lon.in site longitude in decimal degrees
##' @param overwrite logical. Download a fresh version even if a local file with the same name already exists?
##' @param verbose logical. Passed on to \code{\link[ncdf4]{ncvar_def}} and \code{\link[ncdf4]{nc_create}}
##'   to control printing of debug info
##' @param ... Other arguments, currently ignored
##' @export
# -----------------------------------
extract.local.NLDAS <- function(outfolder, in.path, start_date, end_date, lat.in, lon.in, 
                                overwrite = FALSE, verbose = FALSE, ...){

  # Date stuff
  start_date <- as.POSIXlt(start_date, tz = "GMT")
  end_date <- as.POSIXlt(end_date, tz = "GMT")
  start_year <- lubridate::year(start_date)
  end_year   <- lubridate::year(end_date)
  
  lat.in = as.numeric(lat.in)
  lon.in = as.numeric(lon.in)
  # dir.nldas="http://hydro1.sci.gsfc.nasa.gov/thredds/dodsC/NLDAS_FORA0125_H.002"
  dir.create(outfolder, showWarnings=FALSE, recursive=TRUE)
  
  ylist <- seq(start_year,end_year,by=1)
  rows = length(ylist)
  results <- data.frame(file=character(rows), host=character(rows),
                        mimetype=character(rows), formatname=character(rows),
                        startdate=character(rows), enddate=character(rows),
                        dbfile.name = "NLDAS",
                        stringsAsFactors = FALSE
                       )
  
  # I fixed the shortwave radiation parsing script, but haven't pushed those changes, so until NLDAS gets re-formatted, just index it differently
  var = data.frame(NLDAS.name = c("air_temperature","surface_downwelling_longwave_flux_in_air","air_pressure","downwelling_shortwave_flux_in_air","eastward_wind","northward_wind","specific_humidity","precipitation_amount"),
                   CF.name = c("air_temperature","surface_downwelling_longwave_flux_in_air","air_pressure","surface_downwelling_shortwave_flux_in_air","eastward_wind","northward_wind","specific_humidity","precipitation_flux"),
                   units = c('Kelvin',"W/m2","Pascal","W/m2","m/s","m/s","g/g","kg/m2/s") 
                   )
  
  # Progress bar because this can be very slow
  for (i in 1:rows){
    y.now = ylist[i]    
    
    # figure out how many days we're working with
    if(rows>1 & i!=1 & i!=rows){ # If we have multiple years and we're not in the first or last year, we're taking a whole year
      nday  = ifelse(lubridate::leap_year(y.now), 366, 365) # leap year or not; days per year
      day1 = 1
      day2 = nday
      days.use = day1:day2
    } else if(rows==1){
      # if we're working with only 1 year, lets only pull what we need to
      nday  = ifelse(lubridate::leap_year(y.now), 366, 365) # leap year or not; days per year
      day1 <- lubridate::yday(start_date)
      # Now we need to check whether we're ending on the right day
      day2 <- lubridate::yday(end_date)
      days.use = day1:day2
      nday=length(days.use) # Update nday
    } else if(i==1) {
      # If this is the first of many years, we only need to worry about the start date
      nday  = ifelse(lubridate::leap_year(y.now), 366, 365) # leap year or not; days per year
      day1 <- lubridate::yday(start_date)
      day2 = nday
      days.use = day1:day2
      nday=length(days.use) # Update nday
    } else if(i==rows) {
      # If this is the last of many years, we only need to worry about the start date
      nday  = ifelse(lubridate::leap_year(y.now), 366, 365) # leap year or not; days per year
      day1 = 1
      day2 <- lubridate::yday(end_date)
      days.use = day1:day2
      nday=length(days.use) # Update nday
    }
    ntime = nday*24 # leap year or not;time slice (hourly)
    
    loc.file = file.path(outfolder, paste("NLDAS",y.now,"nc",sep="."))
    
    ## Create dimensions
    dim.lat <- ncdf4::ncdim_def(name='latitude', units='degree_north', vals=lat.in, create_dimvar=TRUE)
    dim.lon <- ncdf4::ncdim_def(name='longitude', units='degree_east', vals=lon.in, create_dimvar=TRUE)
    dim.time <- ncdf4::ncdim_def(name='time', units="sec", vals=seq((min(days.use)+1-1/24)*24*360, (max(days.use)+1-1/24)*24*360, length.out=ntime), create_dimvar=TRUE, unlim=TRUE)
    nc.dim=list(dim.lat,dim.lon,dim.time)
    
    var.list = list()
    dat.list = list()
    
    # Defining our dimensions up front
    for(j in 1:nrow(var)){
      var.list[[j]] = ncdf4::ncvar_def(name=as.character(var$CF.name[j]), units=as.character(var$units[j]), dim=nc.dim, missval=-999, verbose=verbose)
      dat.list[[j]] <- array(NA, dim=c(length(lat.in), length(lon.in), ntime)) # Go ahead and make the arrays
    }
    names(var.list) <- names(dat.list) <- var$CF.name
    
    
    # Progress bar just to help us track what's going on
    print("")
    print(y.now)
    pb <- utils::txtProgressBar(min=1, max=nday, style=3)
    pb.index=1
    
    ## get data off OpenDAP
    for(j in 1:length(days.use)){
      utils::setTxtProgressBar(pb, pb.index)
      
      date.now <- as.Date(days.use[j], origin=as.Date(paste0(y.now-1,"-12-31")))
      mo.now <- stringr::str_pad(lubridate::month(date.now), 2, pad="0")
      day.mo <- stringr::str_pad(lubridate::day(date.now), 2, pad="0")
      doy <- stringr::str_pad(days.use[j], 3, pad="0")
      
      # Local netcdf format is 1-file per day
      # NLDAS_FORA0125_H.A19790102.nc
      dap_file <- ncdf4::nc_open(file.path(in.path,y.now,mo.now,paste0("NLDAS_FORA0125_H.A",y.now, mo.now,day.mo,".nc")))
      
      # Query lat/lon
      lats <- ncdf4::ncvar_get(dap_file, "lat")
      lons <- ncdf4::ncvar_get(dap_file, "lon")
      
      # Get the average resolution (without hard-coding and possibly making an error)
      x.inc <- mean(abs(diff(lons)))
      y.inc <- mean(abs(diff(lats)))
      
      lat.use <- which(lats-y.inc/2<=lat.in & lats+y.inc/2>=lat.in)
      lon.use <- which(lons-x.inc/2<=lon.in & lons+x.inc/2>=lon.in)
      
      # Extracting the variables
      for (v in 1:nrow(var)) {
        v.nldas <- paste(var$NLDAS.name[v])
        v.cf    <- paste(var$CF.name   [v])
        
        if(!v.nldas %in% names(dap_file$var) & v.cf %in% names(dap_file$var)) v.nldas <- v.cf
        
        # Variables have different dimensions (which is a pain in the butt)
        # so we need to check to see whether we're pulling 4 dimensions or just 3
        if(dap_file$var[[v.nldas]]$ndims == 4){
          dat.list[[v.cf]][,,(j*24-23):(j*24)] <- ncdf4::ncvar_get(dap_file, v.nldas,
                                                            start=c(lon.use,lat.use,1,1), 
                                                            count=c(1,1,1,24)
          )
        } else {
          dat.list[[v.cf]][,,(j*24-23):(j*24)] <- ncdf4::ncvar_get(dap_file, v.nldas,
                                                            start=c(lon.use,lat.use,1), 
                                                            count=c(1,1,24)
          )
          
        }
      } # end variable loop
      
      ncdf4::nc_close(dap_file) # close file
      pb.index=pb.index+1 # Advance our progress bar
    } # end day
    
    ## change units of precip from kg/m2/hr to kg/m2/s 
    dat.list[["precipitation_flux"]] = dat.list[["precipitation_flux"]]/(60*60)
    
    ## put data in new file
    loc <- ncdf4::nc_create(filename=loc.file, vars=var.list, verbose=verbose)
    for(j in 1:nrow(var)){
      ncdf4::ncvar_put(nc=loc, varid=as.character(var$CF.name[j]), vals=dat.list[[j]])
    }
    ncdf4::nc_close(loc)
    
    results$file[i] <- loc.file
    # results$host[i] <- fqdn()
    results$startdate[i]  <- paste0(as.Date(paste(y.now, day1, sep="-"), format = "%Y-%j"), " 00:00:00")
    results$enddate[i]    <- paste0(as.Date(paste(y.now, day2, sep="-"), format = "%Y-%j"), " 00:00:00")
    results$mimetype[i]   <- 'application/x-netcdf'
    results$formatname[i] <- 'CF Meteorology'
    
  }
  
}
