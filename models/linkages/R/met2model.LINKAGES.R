#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

##-------------------------------------------------------------------------------------------------#
##' Converts a met CF file to a model specific met file. The input
##' files are calld <in.path>/<in.prefix>.YYYY.cf
##'
##' @name met2model.LINKAGES
##' @title Write LINKAGES met files
##' @param in.path path on disk where CF file lives
##' @param in.prefix prefix for each file
##' @param outfolder location where model specific output is written.
##' @return OK if everything was succesful.
##' @export
##' @author Ann Raiho, Betsy Cowdery
##-------------------------------------------------------------------------------------------------#
met2model.LINKAGES <- function(in.path, in.prefix, outfolder, start_date, end_date, ..., overwrite=FALSE,verbose=FALSE) {
  require(PEcAn.utils)
  
  start_date <- as.POSIXlt(start_date, tz = "GMT")
  end_date<- as.POSIXlt(end_date, tz = "GMT")
  out.file <- file.path(outfolder,"climate.Rdata")
  #   out.file <- file.path(outfolder, paste(in.prefix,
  #                                          strptime(start_date, "%Y-%m-%d"),
  #                                          strptime(end_date, "%Y-%m-%d"),
  #                                          "dat", sep="."))
  
  results <- data.frame(file=c(out.file),
                        host=c(fqdn()),
                        mimetype=c('text/plain'),
                        formatname=c('LINKAGES meteorology'),
                        startdate=c(start_date),
                        enddate=c(end_date),
                        dbfile.name = "climate.Rdata",
                        stringsAsFactors = FALSE)
  print("internal results")
  print(results)
  
  if (file.exists(out.file) && !overwrite) {
    logger.debug("File '", out.file, "' already exists, skipping to next file.")
    return(invisible(results))
  }
  
  require(ncdf4)
  require(lubridate)
  require(PEcAn.data.atmosphere)
  #  require(ncdf)
  
  ## check to see if the outfolder is defined, if not create directory for output
  if(!file.exists(outfolder)){
    dir.create(outfolder)
  }
  
  out <- NULL
  
  # get start/end year since inputs are specified on year basis
  start_year <- year(start_date)
  end_year <- year(end_date)
  
  year = sprintf("%04d",seq(start_year,end_year,1))
  month = sprintf("%02d",seq(1,12,1))
  
  nyear = length(year) #number of years to simulate
  
  month_matrix_precip = matrix(NA,nyear,12)
  DOY_vec_hr = c(1,c(32,60,91,121,152,182,213,244,274,305,335,365)*4)
  
  for(i in 1:nyear){     
    ncin <- nc_open(file.path(in.path,paste(in.prefix,year[i],"nc",sep=".")))
    
    ## convert time to seconds
    sec  <- ncin$dim$time$vals  
    sec = udunits2::ud.convert(sec,unlist(strsplit(ncin$dim$time$units," "))[1],"seconds")
    ifelse(leap_year(as.numeric(year[i]))==TRUE,
           dt <- (366*24*60*60)/length(sec), #leap year
           dt <- (365*24*60*60)/length(sec)) #non-leap year
    tstep = 86400/dt
    
    ncprecipf = ncvar_get(ncin, "precipitation_flux")  #units are kg m-2 s-1    
    for(m in 1:12){
      month_matrix_precip[i,m] = (sum(ncprecipf[DOY_vec_hr[m]:(DOY_vec_hr[m+1]-1)]) * dt /10)
    }  
    nc_close(ncin)
   #if(i%%100==0) cat(i," "); flush.console()
  }
    
  month_matrix_temp_mean = matrix(NA,nyear,12)
  
  for(i in 1:nyear){
    ncin <- nc_open(file.path(in.path,paste0(in.prefix,".",year[i],".nc")))
    #print(ncin)
    nctemp = ncvar_get(ncin, "air_temperature") #units are kg m-2 s-1    
    for(m in 1:12){
      month_matrix_temp_mean[i,m] = (mean(nctemp[DOY_vec_hr[m]:(DOY_vec_hr[m+1]-1)]) -273.15) #sub daily to monthly
    } 
    nc_close(ncin)
    if(i%%100==0) cat(i," "); flush.console()
  }

  
  precip.mat <- month_matrix_precip
  temp.mat <- month_matrix_temp_mean
  save(precip.mat, temp.mat,file= out.file)
  invisible(results)
}