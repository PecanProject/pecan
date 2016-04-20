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
  in.path='/fs/data5/pecan.models/LPJ-GUESS/build/CRUNCEP_site_0-622'
  in.prefix='CRUNCEP'
  outfolder='/fs/data5/pecan.models/LPJ-GUESS/build/CRUNCEP_LPJGUESS_site_0-622/'
  start_date='2002/01/01'
  end_date='2002/12/31'
  verbose=FALSE
  
  library(PEcAn.utils)
  require(ncdf4)
  
  print("START met2model.LPJGUESS")
  start_date <- as.POSIXlt(start_date, tz = "GMT")
  end_date <- as.POSIXlt(end_date, tz = "GMT")
  start_year <- year(start_date)
  end_year <- year(end_date)
  
  year = sprintf("%04d",seq(start_year,end_year,1))
  month = sprintf("%02d",seq(1,12,1))
  
  nyear = length(year) #number of years to simulate
  
  ## LPJ-GUESS looks for different input files for different climate variables
  out.files <- list()
  var.names=c("tmp","pre","cld")
  n.var=length(var.names)
  long.names=c("air_temperature","precipitation_flux","surface_downwelling_shortwave_flux_in_air")
  for(i in 1:length(out.names)) out.files[[i]] <- paste(in.prefix, start_year, end_year, var.names[[i]], "nc", sep=".")
  
  
  ## check to see if the outfolder is defined, if not create directory for output
  if(!file.exists(outfolder)){
    dir.create(outfolder)
  }

  month_array=array(NA,dim=c(nyear,12,3))
  DOY_vec_hr = c(1,c(32,60,91,121,152,182,213,244,274,305,335,365)*4)

  
  for(i in 1:nyear){ 
    ## open netcdf file
    ncin <- nc_open(file.path(in.path,paste(in.prefix,year[i],"nc",sep=".")))
    
    lon=ncvar_get(ncin,"longitude")
    lat=ncvar_get(ncin,"latitude")
    
    ## convert time to seconds
    sec   <- ncin$dim$time$vals  
    #sec = udunits2::ud.convert(sec,unlist(strsplit(ncin$dim$time$units," "))[1],"seconds")
    ifelse(leap_year(as.numeric(year[i]))==TRUE,
           dt <- (366*24*60*60)/length(sec), #leap year
           dt <- (365*24*60*60)/length(sec)) #non-leap year
    tstep = 86400/dt
    
    
    ## read climate data
    nc.vars <- list()
    for(v in 1:n.var){
      nc.vars[[v]] = ncvar_get(ncin, long.names[v]) 
      for(m in 1:12) month_array[i,m,v] = (mean(nc.vars[[v]][DOY_vec_hr[m]:(DOY_vec_hr[m+1]-1)])) 
    }

    var.units=c("K","kg m-2 s-1","W m-2")
     
 
    ## write climate data
    ## define dimensions
    latdim <- ncdim_def(name='lat', units='', vals=1:length(lon), create_dimvar=FALSE)
    londim <- ncdim_def(name='lon', units='', vals=1:length(lon), create_dimvar=FALSE)
    timedim=ncdim_def("time", units='',vals=1:12,create_dimvar=FALSE)
    

    fillvalue=9.96920996838687e+36
    


    for(n in 1:n.var) {
      
      var <- ncvar_def(name="time",
                       units="days since 1900-1-1",
                       dim=timedim, missval=as.numeric(-9999))
      
      ncout<- nc_create(filename=file.path(outfolder,paste(out.files[[n]],sep=".")), vars=var, verbose=verbose)
      #ncout<- nc_create(filename=out.files[[n]], vars=var, verbose=verbose)
      ncvar_put(nc=ncout, varid='time', vals=month)
      
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
      ncvar_put(nc=ncout,varid=var.names[n],vals=month_array[,,n])
      ncatt_put(nc=ncout, varid=var.names[n],attname="standard_name", long.names[n])
      
      ncatt_put(nc=ncout, varid="lon",attname="axis", "X")
      ncatt_put(nc=ncout, varid="lon",attname="standard_name", "longitude")
      
      ncatt_put(nc=ncout, varid="lat",attname="axis", "Y")
      ncatt_put(nc=ncout, varid="lat",attname="standard_name", "latitude")
      
      ncatt_put(nc=ncout, varid="time",attname="calendar", "gregorian")
      nc_close(ncout)
    } # end n-loop
    
    ## close netcdf file
    nc_close(ncin)
  } ## end nyear loop
   
  
} ### end met2model.LPJGUESS
