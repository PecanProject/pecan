#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------


### !!! REMOVE AFTER DEBUGGING
#outdir <- "/Volumes/data/sserbin/Modeling/maat/maat_test/out/SA-median/"
#start_date <- '2005-07-15 00:00:00'
#end_date <- '2005-07-15 00:00:01'
#sitelat=46.5
#sitelon=-89.2
#sitelat=-999
#sitelon=-999
### !!!


##-------------------------------------------------------------------------------------------------#
##' Convert MAAT output to netCDF
##'
##' Converts all output contained in a folder to netCDF.
##' @name model2netcdf.MAAT
##' @title Function to convert MAAT model output to standard netCDF format
##' @param outdir Location of MAAT model output
##' @param sitelat Latitude of the site
##' @param sitelon Longitude of the site
##' @param start_date Start time of the simulation
##' @param end_date End time of the simulation
##' @export
##' @author Shawn Serbin, Anthony Walker
model2netcdf.MAAT <- function(outdir, sitelat=-999, sitelon=-999, start_date=NULL, end_date=NULL) {
  
  ## TODO is it OK to give site lat/long -999 if not running at a "site"?
  
  ### Load required libraries
  #require(PEcAn.utils) #nescessary??
  require(ncdf4)
  
  ### Read in model output in SIPNET format
  maat.out.file <- file.path(outdir, "out.csv")
  maat.output <- read.csv(maat.out.file, header=T)
  maat.output.dims <- dim(maat.output)
  
  ### Current runs do not span multiple dates.  Update this code when running over multiple
  ### days and with a explicit timestep. This will occur once use real met data
  days <- as.Date(start_date):as.Date(end_date)
  year <- strftime(as.Date(days,origin="1970-01-01"), "%Y")
  #start <- strftime(as.Date(start_date,origin="1970-01-01"), "%Y-%m-%d")
  #end <- strftime(as.Date(end_date,origin="1970-01-01"), "%Y-%m-%d")
  #start <- as.Date(start_date,"%Y-%m-%d")
  #end <- as.Date(end_date,"%Y-%m-%d")
  num.years <- length(unique(year))
  years <- unique(year)
  timestep.s <- 86400

  ### Setup outputs for netCDF file in appropriate units
  for (y in years){
    #print(y)
    if (file.exists(file.path(outdir, paste(y,"nc", sep=".")))) {
      next ## skip, model output already present.
    }
    
    print(paste("---- Processing year: ", y))  # turn on for debugging
    output <- list()
    
    ### standard variables: Carbon Pools [not currently relevant to MAAT]
    output[[1]] <- y   # Year
    output[[2]] <- (maat.output$A)    # assimilation in umolsC/m2/s - OR KEEP AS ASSIMILATION?
    
    #******************** Declare netCDF variables ********************#
    ## TODO !!!THIS BIT NEEDS UPDATING TO CAPTURE HIGH-FREQUENCY (sub daily) OUTPUTS !!!
    #t <- ncdim_def(name = "time",
    #               units = paste0("days since ", y, "-01-01 00:00:00"),
    #               vals = as.numeric(strptime(end_date, "%Y-%m-%d %H:%M:%S")-strptime(start_date, "%Y-%m-%d %H:%M:%S"),units="days"),
    #               calendar = "standard", unlim = TRUE) # is this correct? fraction of days or whole days
    t <- ncdim_def(name = "time",
                   units = paste0("days since ", y, "-01-01 00:00:00"),
                   vals = 1:nrow(maat.output),
                   calendar = "standard", unlim = TRUE)
    ##
    lat <- ncdim_def("lat", "degrees_east",vals =  as.numeric(sitelat),
                   longname = "station_latitude") 
    lon <- ncdim_def("lon", "degrees_north",vals = as.numeric(sitelon),
                   longname = "station_longitude")
    
    for(i in 1:length(output)){
      if(length(output[[i]])==0) output[[i]] <- rep(-999,length(t$vals))
    }
    
    ### Conversion factor for umol C -> kg C
    Mc <- 12.017 #molar mass of C, g/mol
    umol2kg_C <- Mc * ud.convert(1, "umol", "mol") * ud.convert(1, "g", "kg")
    
    ### Convert outputs
    output <- conversion( 2, umol2kg_C)  ## convert GPP in umolC/m2 s-1 to kgC/m2 s-1 (MsTMIP)
    
    ### Put output into netCDF format
    mstmipvar <- PEcAn.utils::mstmipvar
    var <- list()
    var[[1]]  <- mstmipvar("Year", lat, lon, t, NA)
    var[[2]]  <- mstmipvar("GPP", lat, lon, t, NA)
    
    ### Output netCDF data
    nc <- nc_create(file.path(outdir, paste(y,"nc", sep=".")), var)
    varfile <- file(file.path(outdir, paste(y, "nc", "var", sep=".")), "w")
    for(i in 1:length(var)){
      print(i) # just on for debugging
      ncvar_put(nc,var[[i]],output[[i]])  
      cat(paste(var[[i]]$name, var[[i]]$longname), file=varfile, sep="\n")
    } ## netCDF loop
    close(varfile)
    nc_close(nc)
    
  } ## Year loop
} ## Main loop
##-------------------------------------------------------------------------------------------------#
