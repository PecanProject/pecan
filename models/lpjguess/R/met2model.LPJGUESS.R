#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
#
## R Code to convert NetCDF CF met files into LPJ-GUESS met files

##If files already exist in "Outfolder", the default function is NOT to overwrite them and 
##only gives user the notice that file already exists. If user wants to overwrite the existing files, just change 
##overwrite statement below to TRUE.

##' met2model wrapper for LPJ-GUESS
##'
##' @title met2model.LPJGUESS
##' @export
##' @param in.path location on disk where inputs are stored
##' @param in.prefix prefix of input and output files
##' @param outfolder location on disk where outputs will be stored
##' @param start_date the start date of the data to be downloaded (will only use the year part of the date)
##' @param end_date the end date of the data to be downloaded (will only use the year part of the date)
##' @param overwrite should existing files be overwritten
##' @param verbose should the function be very verbose
##' @author Istem Fer
met2model.LPJGUESS <- function(in.path, in.prefix, outfolder, start_date, end_date, ..., overwrite=FALSE,verbose=FALSE){
  
  ## function arguments for development
  in.path='/fs/data5/pecan.models/LPJ-GUESS/build/NARR_CF_site_0-622'
  in.prefix='NARR'
  outfolder='/fs/data5/pecan.models/LPJ-GUESS/build/NARR_LPJGUESS_site_0-622/'
  start_date='2002/01/01'
  end_date='2003/12/31'
  verbose=FALSE
  
  library(PEcAn.utils)
  require(ncdf4)
  
  print("START met2model.LPJGUESS")
  start_date <- as.POSIXlt(start_date, tz = "GMT")
  end_date <- as.POSIXlt(end_date, tz = "GMT")
  start_year <- year(start_date)
  end_year <- year(end_date)
  
  year = sprintf("%04d",seq(start_year,end_year,1))
  nyear = length(year) #number of years to simulate
  
  ## LPJ-GUESS looks for different input files for different climate variables
  out.files <- list()
  var.names=c("tmp","pre","cld")
  n.var=length(var.names)
  long.names=c("air_temperature","precipitation_flux","surface_downwelling_shortwave_flux_in_air")
  for(i in 1:n.var) out.files[[i]] <- paste(in.prefix, start_year, end_year, var.names[[i]], "nc", sep=".")
  
  
  ## check to see if the outfolder is defined, if not create directory for output
  if(!file.exists(outfolder)){
    dir.create(outfolder)
  }

    ## open netcdf files
    ncin <- lapply(file.path(in.path,paste(in.prefix,year,"nc",sep=".")),nc_open)
    
    ## retrieve lat/lon 
    lon=ncvar_get(ncin[[1]],"longitude")
    lat=ncvar_get(ncin[[1]],"latitude")
    
    ## calculate time step from the time-dimension length, check for leap year
    tstep=ifelse(ncin[[1]]$dim$time$len%%365==0, ncin[[1]]$dim$time$len/365, ncin[[1]]$dim$time$len/366)
    
    ## read climate data
    nc.tmp <- lapply(ncin, ncvar_get, long.names[1])
    nc.pre <- lapply(ncin, ncvar_get, long.names[2])
    nc.cld <- lapply(ncin, ncvar_get, long.names[3])
    
    ## aggregate to daily time steps
    tmp.list <- pre.list <- cld.list <- list()
    for(y in 1:nyear){
      if(as.numeric(year[y])%%4 == 0) ind.vec=rep(1:366,each=tstep) else ind.vec=rep(1:365,each=tstep)
      tmp.list[[y]] <- tapply(nc.tmp[[y]],ind.vec,mean)
      pre.list[[y]] <- tapply(nc.pre[[y]],ind.vec,mean)
      cld.list[[y]] <- tapply(nc.cld[[y]],ind.vec,mean)
    }
    
    var.list=list(unlist(tmp.list),unlist(pre.list),unlist(cld.list))
    
    ## write climate data
    ## define dimensions
    latdim <- ncdim_def(name='lat', units='', vals=1:length(lon), create_dimvar=FALSE)
    londim <- ncdim_def(name='lon', units='', vals=1:length(lon), create_dimvar=FALSE)
    timedim=ncdim_def("time", units='',vals=1:length(var.list[[1]]),create_dimvar=FALSE)
    

    fillvalue=9.96920996838687e+36
    

    for(n in 1:n.var) {
      
      var <- ncvar_def(name="time",
                       units=paste0("days since ",start_year,"-1-1"),
                       dim=timedim, missval=as.numeric(-9999))
      
      ncout<- nc_create(filename=file.path(outfolder,paste(out.files[[n]],sep=".")), vars=var, verbose=verbose)

      ncvar_put(nc=ncout, varid='time', vals=1:length(var.list[[1]]))
      
      var <- ncvar_def(name="lon",
                       units="degree_east",
                       dim=londim, missval=as.numeric(-9999))
      ncout <- ncvar_add(nc=ncout, v=var, verbose=verbose)
      ncvar_put(nc=ncout, varid='lon', vals=lon)
      
      var <- ncvar_def(name="lat",
                       units="degree_north",
                       dim=latdim, missval=as.numeric(-9999))
      ncout<- ncvar_add(nc=ncout, v=var, verbose=verbose)
      ncvar_put(nc=ncout, varid='lat', vals=lat)
      
      var <- ncvar_def(name=var.names[n],units=var.units[n],dim=(list(latdim,londim,timedim)),fillvalue,long.names[n],verbose=verbose,prec="float")
      
      ncout=ncvar_add(nc=ncout, v=var,verbose=verbose)
      ncvar_put(nc=ncout,varid=var.names[n],vals=var.list[[n]])
      ncatt_put(nc=ncout, varid=var.names[n],attname="standard_name", long.names[n])
      
      ncatt_put(nc=ncout, varid="lon",attname="axis", "X")
      ncatt_put(nc=ncout, varid="lon",attname="standard_name", "longitude")
      
      ncatt_put(nc=ncout, varid="lat",attname="axis", "Y")
      ncatt_put(nc=ncout, varid="lat",attname="standard_name", "latitude")
      
      ncatt_put(nc=ncout, varid="time",attname="calendar", "gregorian")
      nc_close(ncout)
    } # end n.var loop
    
    ## close netcdf files
    sapply(ncin,nc_close)

   
  
} ### end met2model.LPJGUESS
