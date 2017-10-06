#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

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
##' 
##' @examples
##' \dontrun{
##' example.output <- system.file("out.csv",package="PEcAn.MAAT")
##' model2netcdf.MAAT(outdir="~/",sitelat=9.154,sitelon=-79.848,start_date="2014-01-01 00:00:00",
##' end_date="2014-12-31 00:00:00")
##' }
##' 
##' @export
##' @author Shawn Serbin, Anthony Walker
##' @importFrom PEcAn.utils misc.convert mstmipvar
##' @importFrom ncdf4 ncdim_def
model2netcdf.MAAT <- function(outdir, sitelat = -999, sitelon = -999, start_date = NULL, end_date = NULL) {

  ## TODO !!UPDATE SO IT WILL WORK WITH NO MET AND WITH MET DRIVER!!

  ### Read in model output in SIPNET format
  maat.out.file <- file.path(outdir, "out.csv")
  maat.output <- read.csv(maat.out.file, header = TRUE)
  maat.output.dims <- dim(maat.output)
  
  ### Determine number of years and output timestep
  days <- as.Date(start_date):as.Date(end_date)
  year <- strftime(as.Date(days, origin = "1970-01-01"), "%Y")
  num.years <- length(unique(year))
  maat.dates <- as.Date(maat.output$time, format = "%m/%d/%y")
  dims <- dim(subset(maat.output,
                     as.Date(time, format = "%m/%d/%y") == seq(as.Date(start_date), by = "days", length = 1)))
  timestep.s <- 86400 / dims[1]
  
  ### Setup outputs for netCDF file in appropriate units
  for (y in unique(year)) {
    if (file.exists(file.path(outdir, paste(y, "nc", sep = ".")))) {
      next  ## skip, model output already present.
    }
    
    print(paste("---- Processing year: ", y))  # turn on for debugging
    
    ## Subset data for processing
    sub.maat.output <- subset(maat.output, format(maat.dates, "%Y") == y)
    sub.maat.dates <- as.Date(sub.maat.output$time, format = "%m/%d/%y")
    sub.maat.doy <- lubridate::yday(sub.maat.dates)
    sub.maat.output.dims <- dim(sub.maat.output)
    dayfrac <- 1 / dims[1]
    day.steps <- seq(0, 0.99, 1 / dims[1])
    
    ### Parse MAAT output
    output      <- list()  # create empty output
    out.year    <- as.numeric(rep(y, sub.maat.output.dims[1]))
    output[[1]] <- out.year  # Simulation year
    output[[2]] <- sub.maat.doy + day.steps  # Fractional day
    output[[3]] <- (sub.maat.output$A)  # assimilation in umols C/m2/s
    output[[4]] <- (sub.maat.output$gs)  # stomatal conductance in mol H2O m-2 s-1
    ## !!TODO: ADD MORE MAAT OUTPUTS HERE!! ##

    #******************** Declare netCDF variables ********************#
    ## This version doesn't provide enough output timesteps when running with met data that has
    ## a step greater than 1 per day
    #t <- ncdf4::ncdim_def(name = "time",
    #               units = paste0("days since ", y, "-01-01 00:00:00"),
    #               vals = as.numeric(strptime(end_date, "%Y-%m-%d %H:%M:%S")-strptime(start_date, "%Y-%m-%d %H:%M:%S"),units="days"),
    #               calendar = "standard", unlim = TRUE) # is this correct? fraction of days or whole days
    
    ## Something like this works for mult timesteps per day
    t <- ncdim_def(name = "time", units = paste0("days since ", y, "-01-01 00:00:00"),
                   vals = sub.maat.doy + day.steps, calendar = "standard", 
                   unlim = TRUE)
    lat <- ncdim_def("lat", "degrees_north", vals = as.numeric(sitelat), longname = "station_latitude")
    lon <- ncdim_def("lon", "degrees_east", vals = as.numeric(sitelon), longname = "station_longitude")
    
    for (i in seq_along(output)) {
      if (length(output[[i]]) == 0) 
        output[[i]] <- rep(-999, length(t$vals))
    }
    
    ### Find/replace missing and convert outputs to standardized BETYdb units
    output[[3]] <- ifelse(output[[3]] == -999, -999, 
                          misc.convert(output[[3]],
                                              "umol C m-2 s-1",
                                              "kg C m-2 s-1"))  # convert A/GPP to kgC/m2/s
    output[[4]] <- ifelse(output[[4]] == "Inf", -999, 
                          misc.convert(output[[4]], 
                                              "mol H2O m-2 s-1",
                                              "kg H2O m-2 s-1"))  # stomatal_conductance in kg H2O m2 s1
    
    dims <- list(lon = lon, lat = lat, time = t)
    
    ### Put output into netCDF format
    nc_var       <- list()
    nc_var[[1]]  <- PEcAn.utils::to_ncvar("Year", dims)
    nc_var[[3]]  <- PEcAn.utils::to_ncvar("GPP", dims)
    nc_var[[4]]  <- PEcAn.utils::to_ncvar("stomatal_conductance", dims)
    
    ### Output netCDF data
    nc <- ncdf4::nc_create(file.path(outdir, paste(y, "nc", sep = ".")), nc_var)
    varfile <- file(file.path(outdir, paste(y, "nc", "var", sep = ".")), "w")
    for (i in seq_along(nc_var)) {
      print(i)  # just on for debugging
      ncdf4::ncvar_put(nc, nc_var[[i]], output[[i]])
      cat(paste(nc_var[[i]]$name, nc_var[[i]]$longname), file = varfile, sep = "\n")
    }  ## netCDF loop
    close(varfile)
    ncdf4::nc_close(nc)
    
  }  ## Year loop
} # model2netcdf.MAAT
##-------------------------------------------------------------------------------------------------#
## EOF