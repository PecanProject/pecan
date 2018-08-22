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
##'
model2netcdf.MAAT <- function(outdir, sitelat = -999, sitelon = -999, start_date = NULL, end_date = NULL) {

  # setup constants
  day_secs <- udunits2::ud.convert(1, "day", "seconds")
  
  ## TODO !!UPDATE SO IT WILL WORK WITH NO MET AND WITH MET DRIVER!!

  ### Read in model output in MAAT format
  maat.out.file <- file.path(outdir, list.files(outdir,'*.csv$')) # updated to handle mod_mimic runs
  maat.output <- utils::read.csv(maat.out.file, header = TRUE, sep = ",")
  maat.output.dims <- dim(maat.output)

  ### Determine number of years and output timestep
  start_year <- lubridate::year(start_date)
  end_year <- lubridate::year(end_date)
  num_years <- length(start_year:end_year)
  # ** maat.dates assumes UTC, is this correct? what if input met is in a local TZ??  need to revist this **
  timezone <- "UTC"  # should be set based on met drivers and what time units they are in.  Ugh
  maat_run_start_date <- format(lubridate::as_datetime(maat.output$time, 
                                                       origin = lubridate::origin, tz =timezone)[1], "%Y-%m-%d %H:%M:%S")
  maat_dates <- strptime(maat.output$time, format = "%Y-%m-%d", tz=timezone)  
  
  ### Setup outputs for netCDF file in appropriate units
  for (year in start_year:end_year) {
    if (file.exists(file.path(outdir, paste(year, "nc", sep = "."))) ) {
      next  ## skip, model output already present.
    }
    
    PEcAn.logger::logger.info(paste("---- Processing MAAT output year: ", year))
    
    ## Subset data for processing
    sub.maat.output <- subset(maat.output, lubridate::year(maat_dates) == year)
    sub.maat.dates <- lubridate::as_date(sub.maat.output$time)
    sub.maat.doy <- lubridate::yday(sub.maat.dates)
    sub.maat.output.dims <- dim(sub.maat.output)
    dims <- dim(subset(sub.maat.output,
                       strptime(time, format = "%Y-%m-%d", tz=timezone) == 
                         seq(strptime(sub.maat.dates[1], format = "%Y-%m-%d", tz=timezone), by = "days", length = 1)))
    timestep.s <- day_secs / dims[1] # e.g. 1800 = 30 minute timesteps
    dayfrac <- 1 / dims[1]
    day.steps <- seq(0, 0.99, 1 / dims[1])
    
    ### Parse MAAT output
    output      <- list()  # create empty output
    out.year    <- as.numeric(rep(year, sub.maat.output.dims[1]))
    output[[1]] <- out.year  # Simulation year
    output[[2]] <- sub.maat.doy + day.steps  # Fractional day
    output[[3]] <- (sub.maat.output$A)  # assimilation in umols C/m2/s
    output[[4]] <- (sub.maat.output$rd)  # respiration in umols C/m2/s
    output[[5]] <- (1/(sub.maat.output$rs))  # stomatal conductance in mol H2O m-2 s-1
    ## !!TODO: ADD MORE MAAT OUTPUTS HERE!! ##

    #******************** Declare netCDF variables ********************#
    t <- ncdf4::ncdim_def(name = "time", units = paste0("days since ", maat_run_start_date),
                   vals = sub.maat.doy + day.steps, calendar = "standard", 
                   unlim = TRUE)  # standard calendar for leap years?  Also need to be sure we update cal depending on leap/no leap
    lat <- ncdf4::ncdim_def("lat", "degrees_north", vals = as.numeric(sitelat), longname = "station_latitude")
    lon <- ncdf4::ncdim_def("lon", "degrees_east", vals = as.numeric(sitelon), longname = "station_longitude")
    
    for (i in seq_along(output)) {
      if (length(output[[i]]) == 0) 
        output[[i]] <- rep(-999, length(t$vals))
    }
    
    ### Find/replace missing and convert outputs to standardized BETYdb units
    output[[3]] <- ifelse(output[[3]] == -999, -999, 
                          PEcAn.utils::misc.convert(output[[3]],
                                              "umol C m-2 s-1",
                                              "kg C m-2 s-1"))  # convert A/GPP to kgC/m2/s
    output[[4]] <- ifelse(output[[4]] == -999, -999, 
                          PEcAn.utils::misc.convert(output[[4]],
                                       "umol C m-2 s-1",
                                       "kg C m-2 s-1"))  # convert leaf resp to kgC/m2/s
    output[[5]][output[[5]]=="Inf"] <- -999
    output[[5]][output[[5]]=="-Inf"] <- -999
    output[[5]] <- ifelse(output[[5]] == -999, -999, 
                          PEcAn.utils::misc.convert(output[[5]], 
                                              "mol H2O m-2 s-1",
                                              "kg H2O m-2 s-1"))  # stomatal_conductance in kg H2O m2 s1

    ### Put output into netCDF format
    dims <- list(lon = lon, lat = lat, time = t) # set dims for netCDF file - REDUNDANT. can be deprecated
    nc_var       <- list()
    nc_var[[1]]  <- ncdf4::ncvar_def("Year", units = "YYYY", dim=list(lat, lon, t), missval = -999, 
                                     longname = "Simulation Year")
    nc_var[[2]]  <- ncdf4::ncvar_def("FracJulianDay", units = "Frac DOY", dim=list(lat, lon, t), missval = -999, 
                                     longname = "Fraction of Julian Date")
    nc_var[[3]]  <- ncdf4::ncvar_def("GPP", units = "kg C m-2 s-1", dim=list(lat, lon, t), missval = -999, 
                                     longname = "Gross Primary Productivity")
    nc_var[[4]]  <- ncdf4::ncvar_def("leaf_respiration", units = "kg C m-2 s-1", dim=list(lat, lon, t), missval = -999, 
                                     longname = "Leaf Respiration Rate")
    nc_var[[5]]  <- ncdf4::ncvar_def("stomatal_conductance", units = "kg H2O m-2 s-1", dim=list(lat, lon, t), missval = -999, 
                                     longname = "Leaf Stomatal Conductance")
    
    ### Output netCDF data
    nc <- ncdf4::nc_create(file.path(outdir, paste(year, "nc", sep = ".")), nc_var)
    varfile <- file(file.path(outdir, paste(year, "nc", "var", sep = ".")), "w")
    for (i in seq_along(nc_var)) {
      #PEcAn.logger::logger.info(paste0("nc file: ",i))  # just on for debugging
      ncdf4::ncvar_put(nc, nc_var[[i]], output[[i]])
      cat(paste(nc_var[[i]]$name, nc_var[[i]]$longname), file = varfile, sep = "\n")
    }  ## netCDF loop
    close(varfile)
    ncdf4::nc_close(nc)
    
  }  ## Year loop
} # model2netcdf.MAAT
##-------------------------------------------------------------------------------------------------#
## EOF
