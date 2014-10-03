#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------#
##' Convert SIPNET output to netCDF
##'
##' Converts all output contained in a folder to netCDF.
##' @name model2netcdf.SIPNET
##' @title Function to convert SIPNET model output to standard netCDF format
##' @param outdir Location of SIPNET model output
##' @param sitelat Latitude of the site
##' @param sitelon Longitude of the site
##' @param start_date Start time of the simulation
##' @param end_date End time of the simulation
##' @export
##' @author Shawn Serbin, Michael Dietze
model2netcdf.SIPNET <- function(outdir, sitelat, sitelon, start_date, end_date) {
  
  require(ncdf4)
  
  ### Read in model output in SIPNET format
  sipnet.output <- read.table(file.path(outdir, "sipnet.out"), header=T, skip=1, sep='')
  sipnet.output.dims <- dim(sipnet.output)

  ### Determine number of years and output timestep
  num.years <- length(unique(sipnet.output$year))
  years <- unique(sipnet.output$year)
  timestep.s <- 86400 / length(which(sipnet.output$year == years[1] & sipnet.output$day == 1))
  out.day <- length(which(sipnet.output$year == years[1] & sipnet.output$day == 1))
  
  ### Loop over years in SIPNET output to create separate netCDF outputs
  for (y in years){
    if (file.exists(file.path(outdir, paste(y,"nc", sep=".")))) {
      next
    }
    print(paste("---- Processing year: ", y))  # turn on for debugging
    
    ## Subset data for processing
    sub.sipnet.output <- subset(sipnet.output, year == y)
    sub.sipnet.output.dims <- dim(sub.sipnet.output)
    dayfrac = 1 / out.day
    step <- seq(0, 0.99, 1 / out.day)

    ## Setup outputs for netCDF file in appropriate units
    output <- list()
    output[[1]] <- sub.sipnet.output$year                       # Year
    output[[2]] <- sub.sipnet.output$day+step                   # Fractional day
    output[[3]] <- (sub.sipnet.output$gpp*0.001)/timestep.s     # GPP in kgC/m2/s
    ## output[[4]] <- (sub.sipnet.output$npp*0.001)/timestep.s     # NPP in kgC/m2/s. Internal SIPNET calculation
    output[[4]] <- (sub.sipnet.output$gpp * 0.001) / timestep.s -
      ((sub.sipnet.output$rAboveground * 0.001)/timestep.s +
      (sub.sipnet.output$rRoot * 0.001) / timestep.s)               # NPP in kgC/m2/s. Post SIPNET calculation
    output[[5]] <- (sub.sipnet.output$rtot * 0.001) / timestep.s    # Total Respiration in kgC/m2/s
    output[[6]] <- (sub.sipnet.output$rAboveground * 0.001) / timestep.s +
      (sub.sipnet.output$rRoot * 0.001) / timestep.s                # Autotrophic Respiration in kgC/m2/s
    output[[7]] <- (sub.sipnet.output$rSoil * 0.001) / timestep.s   # Heterotropic Respiration in kgC/m2/s
    output[[8]] <- (sub.sipnet.output$nee * 0.001) / timestep.s     # NEE in kgC/m2/s
    #output[[9]] <- rep(-999,sipnet.output.dims[1])             # CarbPools
    output[[9]] <- (sub.sipnet.output$plantWoodC * 0.001)         # Above ground wood kgC/m2
    output[[10]] <- (sub.sipnet.output$plantLeafC * 0.001)        # Leaf C kgC/m2
    output[[11]] <- (sub.sipnet.output$plantWoodC * 0.001)+
      (sub.sipnet.output$plantLeafC * 0.001)+
      (sub.sipnet.output$coarseRootC * 0.001)+
      (sub.sipnet.output$fineRootC * 0.001)                       # Total living C kgC/m2
    output[[12]] <- (sub.sipnet.output$soil * 0.001)+
      (sub.sipnet.output$litter * 0.001)                          # Total soil C kgC/m2
    output[[13]] <- (sub.sipnet.output$fluxestranspiration * 0.001) / timestep.s  #Transpiration kgW/m2/s
    output[[14]] <- (sub.sipnet.output$soilWater * 10)            # Soil moisture kgW/m2
    output[[15]] <- (sub.sipnet.output$soilWetnessFrac)         # Fractional soil wetness
    output[[16]] <- (sub.sipnet.output$snow * 10)                 # SWE
    output[[17]] <- sub.sipnet.output$litter * 0.001 ## litter kgC/m2
          
    #******************** Declare netCDF variables ********************#
    t <- ncdim_def(name = "time",
                   units = paste0("days since ", y, "-01-01 00:00:00"),
                   vals = sub.sipnet.output$day - 1 + (sub.sipnet.output$time / 24),
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
    
    mstmipvar <- PEcAn.utils::mstmipvar
    var <- list()
    var[[1]]  <- mstmipvar("Year", lat, lon, t, NA)
    var[[2]]  <- mstmipvar("FracJulianDay", lat, lon, t, NA)
    var[[3]]  <- mstmipvar("GPP", lat, lon, t, NA)
    var[[4]]  <- mstmipvar("NPP", lat, lon, t, NA)
    var[[5]]  <- mstmipvar("TotalResp", lat, lon, t, NA)
    var[[6]]  <- mstmipvar("AutoResp", lat, lon, t, NA)
    var[[7]]  <- mstmipvar("HeteroResp", lat, lon, t, NA)
    var[[8]]  <- mstmipvar("NEE", lat, lon, t, NA)
    #var[[9]]  <- mstmipvar("CarbPools", lat, lon, t, NA)
    var[[9]]  <- mstmipvar("AbvGrndWood", lat, lon, t, NA)
    var[[10]]  <- mstmipvar("LeafC", lat, lon, t, NA)
    var[[11]]  <- mstmipvar("TotLivBiom", lat, lon, t, NA)
    var[[12]]  <- mstmipvar("TotSoilCarb", lat, lon, t, NA)
    var[[13]]  <- mstmipvar("TVeg", lat, lon, t, NA)
    var[[14]]  <- mstmipvar("SoilMoist", lat, lon, t, NA)
    var[[15]]  <- mstmipvar("SoilMoistFrac", lat, lon, t, NA)
    var[[16]]  <- mstmipvar("SWE", lat, lon, t, NA)
    var[[17]]  <- mstmipvar("Litter", lat, lon, t, NA)
   
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
