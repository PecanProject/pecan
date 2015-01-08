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
##' @name met2model.MODEL
##' @title Write MODEL met files
##' @param in.path path on disk where CF file lives
##' @param in.prefix prefix for each file
##' @param outfolder location where model specific output is written.
##' @return OK if everything was succesful.
##' @export
##' @author Rob Kooper
##-------------------------------------------------------------------------------------------------#
met2model.LINKAGES <- function(in.path, in.prefix, outfolder, overwrite=FALSE) {
  logger.severe("NOT IMPLEMENTED")
  
  setwd("/Users/paleolab/Linkages/PalEON.Met/PHA/pecan2.bu.edu/input/PalEON.MIP.PBL")
  
  year = seq(850,2010,1)
  month = seq(1,12,1)

  month_matrix_precip = matrix(NA,length(year),12)
  #month_vec = c(31,28,31,30,31,30,31,31,30,31,30,31)
  julian_vec_hr = c(1,c(32,60,91,121,152,182,213,244,274,305,335,365)*4)
  
  for(i in 1:length(year)){
    ncin <- nc_open(paste(year[i],".nc",sep=""))
    #print(ncin)
    ncprecipf = ncvar_get(ncin, "precipitation_flux") #units are kg m-2 s-1    
    for(m in 1:12){
      month_matrix_precip[i,m] = sum(ncprecipf[julian_vec_hr[m]:(julian_vec_hr[m+1]-1)]) * 21600^2 #fix when Mike changes code
    } 
    nc_close(ncin)
    if(i%%100==0) cat(i," "); flush.console()
  }
  
  nyear = 1150 #number of years to simulate
  ipolat_nums = seq(2,nyear,2) #years for climate interpolation
  mean_ncprecipf_mm = matrix(0,length(ipolat_nums),12) ; mean_ncprecipf_cm = mean_ncprecipf_mm; sd_ncprecipf_mm = mean_ncprecipf_mm ;sd_ncprecipf_cm = mean_ncprecipf_mm
  
  for(i in 1:length(ipolat_nums)){
    for(m in 1:12){
      mean_ncprecipf_mm[i,m] = mean(month_matrix[ipolat_nums[i],m],month_matrix[ipolat_nums[i]-1,m])
      sd_ncprecipf_mm[i,m] = sd(c(month_matrix[ipolat_nums[i],m],month_matrix[ipolat_nums[i]-1,m]))
    }  
  }
  
  mean_ncprecipf_cm <- (mean_ncprecipf_mm / 10)
  sd_ncprecipf_cm <- (sd_ncprecipf_mm / 10)
  
  month_matrix_temp_mean = matrix(NA,length(year),12)
  
  for(i in 1:length(year)){
    ncin <- nc_open(paste(year[i],".nc",sep=""))
    #print(ncin)
    nctemp = ncvar_get(ncin, "air_temperature") #units are kg m-2 s-1    
    for(m in 1:12){
      month_matrix_temp_mean[i,m] = mean(nctemp[julian_vec_hr[m]:(julian_vec_hr[m+1]-1)]) #sub daily to monthly
    } 
    nc_close(ncin)
    if(i%%100==0) cat(i," "); flush.console()
  }
  
  nyear = 1150 #number of years to simulate
  ipolat_nums = seq(2,nyear,2) #years for climate interpolation
  mean_nctemp = matrix(0,length(ipolat_nums),12) ; sd_nctemp = mean_nctemp
  for(i in 1:length(ipolat_nums)){
    for(m in 1:12){
      mean_nctemp[i,m] = mean(month_matrix_temp_mean[ipolat_nums[i],m],month_matrix_temp_mean[ipolat_nums[i]-1,m])
      sd_nctemp[i,m] = sd(c(month_matrix_temp_mean[ipolat_nums[i],m],month_matrix_temp_mean[ipolat_nums[i]-1,m]))
    }  
  }
  
  mean_nctemp_C <- (mean_nctemp - 273.15)
  sd_nctemp_C <- sd_nctemp
  
  
  
}
