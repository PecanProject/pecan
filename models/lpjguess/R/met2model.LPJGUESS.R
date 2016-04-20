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
  
  library(PEcAn.utils)
  require(ncdf4)
  
  print("START met2model.LPJGUESS")
  start_date <- as.POSIXlt(start_date, tz = "GMT")
  end_date <- as.POSIXlt(end_date, tz = "GMT")
  start_year <- year(start_date)
  end_year <- year(end_date)
  
  year = sprintf("%04d",seq(start_year,end_year,1))
  #month = sprintf("%02d",seq(1,12,1))
  
  nyear = length(year) #number of years to simulate
  
  ## LPJ-GUESS looks for different input files for different climate variables
  out.files <- list()
  out.names=c("tmp","pre","cld")
  for(i in 1:length(out.names)) out.files[[i]] <- paste(in.prefix, start_year, end_year, out.names[[i]], "nc", sep=".")
  
  month_matrix_tmp = matrix(NA,nyear,12)
  month_matrix_pre = matrix(NA,nyear,12)
  month_matrix_cld = matrix(NA,nyear,12)
  DOY_vec_hr = c(1,c(32,60,91,121,152,182,213,244,274,305,335,365)*4)

  
  for(i in 1:nyear){ 
    ## open netcdf file
    ncin <- nc_open(file.path(in.path,paste(in.prefix,year[i],"nc",sep=".")))
    
    ## convert time to seconds
    sec   <- ncin$dim$time$vals  
    #sec = udunits2::ud.convert(sec,unlist(strsplit(ncin$dim$time$units," "))[1],"seconds")
    ifelse(leap_year(as.numeric(year[i]))==TRUE,
           dt <- (366*24*60*60)/length(sec), #leap year
           dt <- (365*24*60*60)/length(sec)) #non-leap year
    tstep = 86400/dt
    
    nctmp = ncvar_get(ncin, "air_temperature") #units are K 
    ncpre = ncvar_get(ncin, "precipitation_flux") #units are kg m-2 s-1  
    nccld = ncvar_get(ncin, "surface_downwelling_shortwave_flux_in_air") #units are W m-2
    
    for(m in 1:12){
      month_matrix_tmp[i,m] = (mean(nctmp[DOY_vec_hr[m]:(DOY_vec_hr[m+1]-1)])) 
      month_matrix_pre[i,m] = (sum(ncpre[DOY_vec_hr[m]:(DOY_vec_hr[m+1]-1)]) * dt /10)
      month_matrix_cld[i,m] = (mean(nccld[DOY_vec_hr[m]:(DOY_vec_hr[m+1]-1)])) 
    }  
    
    ## close netcdf file
    nc_close(ncin)
  } ## end nyear for loop
   
} ### end met2model.LPJGUESS
