#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

##-------------------------------------------------------------------------------------------------#
##' Convert MODEL output into the NACP Intercomparison format (ALMA using netCDF)
##' 
##' @name model2netcdf.MODEL
##' @title Code to convert MODELS's output into netCDF format
##'
##' @param outdir Location of model output
##' @param sitelat Latitude of the site
##' @param sitelon Longitude of the site
##' @param start_date Start time of the simulation
##' @param end_date End time of the simulation
##' @export
##'
##' @author Tony Gardella
model2netcdf.PRELES <- function(outdir, sitelat, sitelon, start_date, end_date) {
  
  require(PEcAn.utils)
  require(ncdf4)
  
  ### Read in model output in PRELES format
  PRELES.output <- preles_out
  PRELES.output.dims <- dim(PRELES.output)
  
  ### Determine number of years and output timestep
  days = as.Date(start_date):as.Date(end_date)
  year = strftime(as.Date(days,origin="1970-01-01"), "%Y")
  num.years <- length(unique(year))
  years <- unique(year)
  timestep.s <- 86400
  
  ### Loop over years in PRELES output to create separate netCDF outputs
  for (y in years){
    if (file.exists(file.path(outdir, paste(y,"nc", sep=".")))) {
      next
    }
    print(paste("---- Processing year: ", y))  # turn on for debugging
    
    ## Subset data for processing
    sub.PRELES.output <- subset(PRELES.output, year == y)
    sub.PRELES.output.dims <- dim(sub.PRELES.output)
    
    ## Setup outputs for netCDF file in appropriate units
    output <- list()
    ## standard variables
    output[[1]] <- (sub.PRELES.output[,31]*0.001)/timestep.s     # GPP in kgC/m2/s    
    output[[2]] <- (sub.PRELES.output[,33]* 0.001) / timestep.s  # Evapotranspiration in kgCm2/s
    output[[3]] <- (sub.PRELES.output[,7]*0.001)     # SoilMoist kg/m2
    output[[4]] <- (sub.PRELES.output[,]) # unitless modifier
    output[[5]] <- (sub.PRELES.output[,]) # unitless modifier
    output[[6]] <- (sub.PRELES.output[,]* 0.001)/ timestep.s # Evaporation Evap kg/m2/s
    output[[7]] <- (sub.PRELES.output[,]* 0.001)/ timestep.s # Transpiration TVeg kg/m2s
    #******************** Declare netCDF variables ********************#
    t <- ncdim_def(name = "time",
                   units = paste0("days since ", y, "-01-01 00:00:00"),
                   vals = 1:nrow(sub.PRELES.output),
                   calendar = "standard", unlim = TRUE)
    lat <- ncdim_def("lat", "degrees_east",
                     vals =  as.numeric(sitelat),
                     longname = "station_latitude") 
    lon <- ncdim_def("lon", "degrees_north",
                     vals = as.numeric(sitelon),
                     longname = "station_longitude")
    
    ## ***** Need to dynamically update the UTC offset here *****
    
    for(i in 1:length(output)){
      if(length(output[[i]])==0) output[[i]] <- rep(-999,length(t$vals))
    }
    
    var <- list()
    var[[1]]  <- mstmipvar("GPP", lat, lon, t, NA)
    var[[2]]  <- ncvar_def("Evpotranspiration", "kg/m2/s", list(lon,lat,t), -999)
    var[[3]]  <- mstmipvar("SoilMoist", lat, lon, t, NA)
    var[[4]]  <- ncvar_def("fWE", , list(lon,lat,t), -999)
    var[[5]]  <- ncvar_def("fW", , list(lon,lat,t), -999)
    var[[6]]  <- mstmipvar("Evap", "kg/m2/s", list(lon,lat,t), -999)
    var[[7]]  <- mstmipvar("TVeg", "kg/m2/s", list(lon,lat,t), -999)
    
    

    
    #******************** Declar netCDF variables ********************#
    
    
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
