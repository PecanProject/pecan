## Copyright (c) 2012 University of Illinois, NCSA.
## All rights reserved. This program and the accompanying materials
## are made available under the terms of the 
## University of Illinois/NCSA Open Source License
## which accompanies this distribution, and is available at
## http://opensource.ncsa.illinois.edu/license.html
## ---------------------------------------------------------------------
##' Convert LPJ-GUESS output to netCDF
##' 
##' @name model2netcdf.LPJGUESS
##' @title Function to convert LPJ-GUESS model output to standard netCDF format
##'
##' @param outdir Location of LPJ-GUESS model output
##' @param sitelat Latitude of the site
##' @param sitelon Longitude of the site
##' @param start_date Start time of the simulation
##' @param end_date End time of the simulation
##' @export
##'
##' @author Istem Fer

model2netcdf.LPJGUESS <- function(outdir, sitelat, sitelon, start_date, end_date) {
  
  require(ncdf4)
  require(lubridate)
  
  ### Read in model output in LPJ-GUESS format
  lpjguess.out.files <- list.files(outdir)
  lpjguess.out.files =lpjguess.out.files[-(which(lpjguess.out.files %in% "logfile.txt"))]

  
  if(length(lpjguess.out.files)==0)  logger.error("No output files found at ", outdir)
  
  lpjguess.output <- lapply(file.path(outdir,lpjguess.out.files), read.table, header=TRUE, sep="")
  #n.outputs <- length(lpjguess.output)
  m.to.s=2592000
  
  years=seq(year(start_date),year(end_date))
  
  
  ### Unit conversions
  
  # mgpp "monthly gross primary production" in kgC/m2/month to GPP kgC/m2/s
  if("mgpp.out" %in% lpjguess.out.files) gpp=lpjguess.output[[which(lpjguess.out.files=="mgpp.out")]][,4:15]/m.to.s
  
  # mnpp "monthly net primary production" in kgC/m2/month to NPP kgC/m2/s
  if("mnpp.out" %in% lpjguess.out.files) npp=lpjguess.output[[which(lpjguess.out.files=="mnpp.out")]][,4:15]/m.to.s
  
  # mra "monthly autotrophic respiration" in kgC/m2/month to AutoResp kgC/m2/s
  if("mra.out" %in% lpjguess.out.files) arp=lpjguess.output[[which(lpjguess.out.files=="mra.out")]][,4:15]/m.to.s
  
  # mrh "monthly heterotrophic respiration" in kgC/m2/month to HeteroResp kgC/m2/s
  if("mrh.out" %in% lpjguess.out.files) hrp=lpjguess.output[[which(lpjguess.out.files=="mrh.out")]][,4:15]/m.to.s
  
  # mnee "monthly net ecosystem C exchange" in kgC/m2/month to NEE kgC/m2/s
  if("mnee.out" %in% lpjguess.out.files) nee=lpjguess.output[[which(lpjguess.out.files=="mnee.out")]][,4:15]/m.to.s
  
  # mlai "monthly Leaf Area Index" in m2/m2 to LAI m2/m2
  if("mnee.out" %in% lpjguess.out.files) lai=lpjguess.output[[which(lpjguess.out.files=="mlai.out")]][,4:15]
  
  
  
  ### Loop over years in LPJ-GUESS output to create separate netCDF outputs
  for (y in years){
    
    if (file.exists(file.path(outdir, paste(y,"nc", sep=".")))) {
      next
    }
    
    print(paste("---- Processing year: ", y))
    
    ## Setup outputs for netCDF file in appropriate units
    
    ## TODO: generalize for all possible outputs, both yearly and monthly
    
    output <- list()

    output[[1]] = gpp[which(years==y),] # GPP in kgC/m2/s
    output[[2]] = npp[which(years==y),] # NPP in kgC/m2/s
    output[[3]] = arp[which(years==y),] # AutoResp in kgC/m2/s
    output[[4]] = hrp[which(years==y),] # HeteroResp in kgC/m2/s
    output[[5]] = nee[which(years==y),] # NEE in kgC/m2/s
    output[[6]] = lai[which(years==y),] # LAI in m2/m2


    
    
    #******************** Declare netCDF dimensions and variables ********************#
    t <- ncdim_def(name = "time",
                   units = paste0("days since ", y, "-01-01 00:00:00"),
                   vals = 1:12,
                   calendar = "standard", unlim = TRUE)
    lat <- ncdim_def("lat", "degrees_east",
                     vals =  as.numeric(sitelat),
                     longname = "station_latitude") 
    lon <- ncdim_def("lon", "degrees_north",
                     vals = as.numeric(sitelon),
                     longname = "station_longitude")
    
    
    mstmipvar <- PEcAn.utils::mstmipvar
    
    var <- list()

    var[[1]]  <- mstmipvar("GPP", lat, lon, t, NA)
    var[[2]]  <- mstmipvar("NPP", lat, lon, t, NA)
    var[[3]]  <- mstmipvar("AutoResp", lat, lon, t, NA)
    var[[4]]  <- mstmipvar("HeteroResp", lat, lon, t, NA)
    var[[5]]  <- mstmipvar("NEE", lat, lon, t, NA)
    var[[6]]  <- mstmipvar("LAI", lat, lon, t, NA)
    
    #******************** Declare netCDF variables ********************#
    
    
    ### Output netCDF data
    nc <- nc_create(file.path(outdir, paste(y,"nc", sep=".")), var)
    varfile <- file(file.path(outdir, paste(y, "nc", "var", sep=".")), "w")
    for(i in 1:length(var)){
      #print(i)
      ncvar_put(nc,var[[i]],output[[i]])  
      cat(paste(var[[i]]$name, var[[i]]$longname), file=varfile, sep="\n")
    }
    close(varfile)
    nc_close(nc)
    
  } ### End of year loop
  
  
} ### End of function
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.              
####################################################################################################