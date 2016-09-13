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
#outdir <- "/data/sserbin/Modeling/maat/maat_test/out/SA-median/"
#start_date <- '2005-07-15 00:00:00'
#end_date <- '2005-07-15 00:00:01'
#sitelat=46.5
#sitelon=-89.2
#y="2005"
#sitelat=-999
#sitelon=-999

#outdir <- "/data/sserbin/Modeling/maat/maat_met_tests.4/out/SA-median/"
#start_date <- '2006-01-01 00:00:00'
#end_date <- '2006-12-31 00:00:00'
#sitelat <- 39.9712
#sitelon <- -74.4346
#y <- "2006"

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
  require(udunits2)
  require(ncdf4)
  
  ### Read in model output in SIPNET format
  maat.out.file <- file.path(outdir, "out.csv")
  maat.output <- read.csv(maat.out.file, header=T)
  maat.output.dims <- dim(maat.output)

  ### Determine number of years and output timestep
  days <- as.Date(start_date):as.Date(end_date)
  year <- strftime(as.Date(days,origin="1970-01-01"), "%Y")
  num.years <- length(unique(year))
  timestep.s <- 86400 / 48  # initially hard coded at half hour time steps

  ### Setup outputs for netCDF file in appropriate units
  for (y in unique(year)){
    if (file.exists(file.path(outdir, paste(y,"nc", sep=".")))) {
      next ## skip, model output already present.
    }
    
    print(paste("---- Processing year: ", y))  # turn on for debugging

    
    ## Subset data for processing - !!! NEED TO ADD YEAR TO OUTPUT FILE TO SUBSET OVER MULT YEARS
    #sub.maat.output <- subset(maat.output, year == y)
    #sub.maat.output.dims <- dim(sub.maat.output)
    #dayfrac = 1 / out.day
    #step <- seq(0, 0.99, 1 / out.day)

    ### standard variables: Carbon Pools [not currently relevant to MAAT]
    #output[[1]] <- y   # Year - REMOVE!  ALREADY IN THE NETCDF AS TIME!!
    #output[[1]] <- year
    output <- list()
    out.year <- rep(y,maat.output.dims[1])
    output[[1]] <- out.year           # Simulation year
    #output[[2]] <-                   # Fractional day - NEED TO IMPLEMENT  
    output[[2]] <- (maat.output$A)    # assimilation in umolsC/m2/s - OR KEEP AS ASSIMILATION?
    #output[[2]] <- c(maat.output$A,-999)
    
    #******************** Declare netCDF variables ********************#
    ## TODO !!!THIS BIT NEEDS UPDATING TO CAPTURE HIGH-FREQUENCY (sub daily) OUTPUTS !!!
    #t <- ncdim_def(name = "time",
    #               units = paste0("days since ", y, "-01-01 00:00:00"),
    #               vals = as.numeric(strptime(end_date, "%Y-%m-%d %H:%M:%S")-strptime(start_date, "%Y-%m-%d %H:%M:%S"),units="days"),
    #               calendar = "standard", unlim = TRUE) # is this correct? fraction of days or whole days
    #t <- ncdim_def(name = "time",
    #               units = paste0("days since ", y, "-01-01 00:00:00"),
    #               vals = 1:nrow(maat.output),
    #               calendar = "standard", unlim = TRUE)
    t <- ncdim_def(name = "time",
                   units = paste0("days since ", y, "-01-01 00:00:00"),
                   vals = seq(365/17520,365,365/17520),
                   calendar = "standard", unlim = TRUE) # is this correct? fraction of days or whole days
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
    #output <- conversion( 2, umol2kg_C)  ## convert GPP in umolC/m2 s-1 to kgC/m2 s-1 (MsTMIP)
    #output[[2]][output[[2]] != -999] <- output[[2]][output[[2]] != -999] * umol2kg_C
    output[[2]] <- ifelse(output[[2]]==-999,-999,output[[2]]*umol2kg_C)
    
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
