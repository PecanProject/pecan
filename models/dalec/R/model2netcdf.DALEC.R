#-------------------------------------------------------------------------------
# Copyright (c) 2012 NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------#
##' Convert DALEC output to netCDF
##'
##' Converts all output contained in a folder to netCDF.
##' @name model2netcdf.DALEC
##' @title Function to convert DALEC model output to standard netCDF format
##' @param outdir Location of DALEC model output
##' @param sitelat Latitude of the site
##' @param sitelon Longitude of the site
##' @param start_date Start time of the simulation
##' @param end_date End time of the simulation
##' @export
##' @author Shawn Serbin, Michael Dietze
model2netcdf.DALEC <- function(outdir, sitelat, sitelon, start_date, end_date) {
  
  ### Read in model output in DALEC format
  DALEC.output <- read.table(file.path(outdir, "out.txt"), header=FALSE,sep='')
  DALEC.output.dims <- dim(DALEC.output)

  ### Determine number of years and output timestep
  days = as.Date(start_date):as.Date(end_date)
  year = strftime(as.Date(days,origin="1970-01-01"), "%Y")
  num.years <- length(unique(year))
  years <- unique(year)
  timestep.s <- 86400
  
  ### Loop over years in DALEC output to create separate netCDF outputs
  for (y in years){
    if (file.exists(file.path(outdir, paste(y,"nc", sep=".")))) {
      next
    }
    print(paste("---- Processing year: ", y))  # turn on for debugging
    
    ## Subset data for processing
    sub.DALEC.output <- subset(DALEC.output, year == y)
    sub.DALEC.output.dims <- dim(sub.DALEC.output)

    ## Setup outputs for netCDF file in appropriate units
    output <- list()
    ## standard variables: Fluxes
    output[[1]] <- (sub.DALEC.output[,1] * 0.001) / timestep.s # Autotrophic Respiration in kgC/m2/s
    output[[2]] <- (sub.DALEC.output[,21] + sub.DALEC.output[,23])*0.001/timestep.s # Heterotrophic Resp kgC/m2/s
    output[[3]] <- (sub.DALEC.output[,31]*0.001)/timestep.s     # GPP in kgC/m2/s    
    output[[4]] <- (sub.DALEC.output[,33]* 0.001) / timestep.s  # NEE in kgC/m2/s
    output[[5]] <- (sub.DALEC.output[,3] + sub.DALEC.output[,5] 
                    + sub.DALEC.output[,7])*0.001/timestep.s # NPP kgC/m2/s
    
    ## non-standard variables: Fluxes
    output[[6]] <- (sub.DALEC.output[,9]* 0.001) / timestep.s   # Leaf Litter, kgC/m2/s
    output[[7]] <- (sub.DALEC.output[,11]* 0.001) / timestep.s  # Woody Litter, kgC/m2/s
    output[[8]] <- (sub.DALEC.output[,13]* 0.001) / timestep.s  # Root Litter, kgC/m2/s
    
    ## non-standard variables: Pools
    output[[9]]  <- (sub.DALEC.output[,15]* 0.001)    # Leaf Biomass, kgC/m2
    output[[10]] <- (sub.DALEC.output[,17]* 0.001)    # Wood Biomass, kgC/m2
    output[[11]] <- (sub.DALEC.output[,19]* 0.001)    # Root Biomass, kgC/m2
    output[[12]] <- (sub.DALEC.output[,27]* 0.001)    # Litter Biomass, kgC/m2
    output[[13]] <- (sub.DALEC.output[,29]* 0.001)    # Soil C, kgC/m2
    
    ## standard composites
    output[[14]] = output[[1]] + output[[2]] # Total Respiration
    output[[15]] = output[[9]] + output[[10]] + output[[11]] ## TotLivBiom
    output[[16]] = output[[12]] + output[[13]] ## TotSoilCarb
          
    #******************** Declare netCDF variables ********************#
    t <- ncdim_def(name = "time",
                   units = paste0("days since ", y, "-01-01 00:00:00"),
                   vals = 1:nrow(sub.DALEC.output),
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
   var[[1]]  <- mstmipvar("AutoResp", lat, lon, t, NA)
   var[[2]]  <- mstmipvar("HeteroResp", lat, lon, t, NA)
   var[[3]]  <- mstmipvar("GPP", lat, lon, t, NA)
   var[[4]]  <- mstmipvar("NEE", lat, lon, t, NA) 
   var[[5]]  <- mstmipvar("NPP", lat, lon, t, NA)
   
   var[[6]]  <- ncvar_def("LeafLitter", "kgC/m2/s", list(lon,lat,t), -999)
   var[[7]]  <- ncvar_def("WoodyLitter", "kgC/m2/s", list(lon,lat,t), -999)
   var[[8]]  <- ncvar_def("RootLitter", "kgC/m2/s", list(lon,lat,t), -999)
   var[[9]]  <- ncvar_def("LeafBiomass", "kgC/m2", list(lon,lat,t), -999)
   var[[10]]  <- ncvar_def("WoodBiomass", "kgC/m2", list(lon,lat,t), -999)
   var[[11]]  <- ncvar_def("RootBiomass", "kgC/m2", list(lon,lat,t), -999)
   var[[12]]  <- ncvar_def("LitterBiomass", "kgC/m2", list(lon,lat,t), -999)
   var[[13]]  <- ncvar_def("SoilC", "kgC/m2", list(lon,lat,t), -999)
   
    var[[14]]  <- mstmipvar("TotalResp", lat, lon, t, NA)
    var[[15]]  <- mstmipvar("TotLivBiom", lat, lon, t, NA)
    var[[16]]  <- mstmipvar("TotSoilCarb", lat, lon, t, NA)
    
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
