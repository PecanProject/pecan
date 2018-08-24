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
##' @param rundir Location of MAAT model run (i.e. MAAT project) directory with all required model run inputs.
##' This is needed to identify model runs with and without met drivers and control the model output conversion process
##' @param outdir Location of MAAT model output
##' @param sitelat Latitude of the site
##' @param sitelon Longitude of the site
##' @param start_date Start time of the simulation
##' @param end_date End time of the simulation
##' 
##' @examples
##' \dontrun{
##' example.output <- system.file("out.csv",package="PEcAn.MAAT")
##' model2netcdf.MAAT(rundir="~/scratch/run/", outdir="~/", sitelat=9.154, sitelon=-79.848, 
##' start_date="2014-01-01 00:00:00", end_date="2014-12-31 00:00:00")
##' }
##' 
##' @export
##' @author Shawn Serbin, Anthony Walker
##'
model2netcdf.MAAT <- function(rundir, outdir, sitelat = -999, sitelon = -999, start_date = NULL, end_date = NULL) {

  # setup constants
  day_secs <- udunits2::ud.convert(1, "day", "seconds")
  
  # setup helper function
  var_update <- function(data, out, oldname, newname, oldunits, newunits=NULL, missval=-999, longname, ncdims) {
    ## define variable
    if(is.null(newunits)) newunits = oldunits
    newvar <- ncdf4::ncvar_def(name = newname, units = newunits, dim = ncdims, missval=missval, longname=longname)
    ## convert data
    dat <- data
    if (newname %in% c("Year","FracJulianDay")) {
      PEcAn.logger::logger.info(paste0("Skipping conversion for: ", newname))
      dat.new <- dat
    } else {
      dat.new <- try(PEcAn.utils::misc.convert(dat,oldunits,newunits),silent = TRUE)
    }
    ## prep for writing
    if(is.null(out)) {
      out <- list(var <- list(),dat <- list())
      out$var[[1]] <- newvar
      out$dat[[1]] <- dat.new
    } else {
      i <- length(out$var) + 1
      out$var[[i]] <- newvar
      out$dat[[i]] <- dat.new
    }
    return(out)
  }
  
  ### look for leaf_user_met.xml file
  met_exists <- file.exists(file.path(rundir,"leaf_user_met.xml"))

  ## TODO: Clean up and make this function more elegant.  In particular, refactor such that its easier to 
  ## manage output variables when running with/without met drivers vs have two separate processing paths below
  
  ### Read in model output in MAAT format
  maat.out.file <- file.path(outdir, list.files(outdir,'*.csv$')) # updated to handle mod_mimic runs
  maat.output <- utils::read.csv(maat.out.file, header = TRUE, sep = ",")
  maat.output.dims <- dim(maat.output)

  ### Determine number of years and output timestep
  start_year <- lubridate::year(start_date)
  end_year <- lubridate::year(end_date)
  num_years <- length(start_year:end_year)
  timezone <- "UTC"  # should be set based on met drivers and what time units they are in.  Ugh
  
  if (met_exists) {
    # ** maat.dates assumes UTC, is this correct? what if input met is in a local TZ??  need to revist this **
    maat_run_start_date <- format(lubridate::as_datetime(maat.output$time, tz =timezone)[1], "%Y-%m-%d %H:%M:%S")
    maat_dates <- strptime(maat.output$time, format = "%Y-%m-%d", tz=timezone)  
  } else {
    maat_run_start_date <- format(lubridate::as_datetime(start_date, tz =timezone)[1], "%Y-%m-%d %H:%M:%S")
  }
  
  ### setup nc file lat/long
  lat <- ncdf4::ncdim_def("lat", "degrees_north", vals = as.numeric(sitelat), longname = "station_latitude")
  lon <- ncdf4::ncdim_def("lon", "degrees_east", vals = as.numeric(sitelon), longname = "station_longitude")
  
  ### Setup outputs for netCDF file in appropriate units
  for (year in start_year:end_year) {
    if (file.exists(file.path(outdir, paste(year, "nc", sep = "."))) ) {
      next  ## skip, model output already present.
    }
    
    PEcAn.logger::logger.info(paste("---- Processing MAAT output year: ", year))
    
    if (met_exists) {
      
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
      
      # setup netCDF time variable for year
      t <- ncdf4::ncdim_def(name = "time", units = paste0("days since ", maat_run_start_date),
                            vals = sub.maat.doy + day.steps, calendar = "standard", 
                            unlim = TRUE)  # standard calendar for leap years?  Also need to be sure we update cal depending on leap/no leap
      
      ### Parse MAAT output
      #output      <- list()  # create empty output
      output <- NULL
      ncdims <- list(lon, lat, t)
      out.year <- as.numeric(rep(year, sub.maat.output.dims[1]))
      output <- var_update(out.year, output, "Year", "Year", oldunits='YYYY', newunits=NULL, missval=-999, 
                           longname="Simulation Year", ncdims=ncdims)
      output <- var_update(sub.maat.doy + day.steps, output, "FracJulianDay", "FracJulianDay", oldunits='Frac DOY', newunits=NULL, missval=-999, 
                           longname="Fraction of Julian Date", ncdims=ncdims)
      output <- var_update(sub.maat.output$A, output, "A", "GPP", oldunits="umol C m-2 s-1", newunits="kg C m-2 s-1", missval=-999, 
                           longname="Gross Primary Productivity", ncdims=ncdims)
      output <- var_update(sub.maat.output$rd, output, "rd", "leaf_respiration", oldunits="umol C m-2 s-1", newunits="kg C m-2 s-1", missval=-999, 
                           longname="Leaf Respiration Rate", ncdims=ncdims)
      output <- var_update((1/(sub.maat.output$rs)), output, "gs", "stomatal_conductance", oldunits="mol H2O m-2 s-1", 
                           newunits="kg H2O m-2 s-1", missval=-999, longname="Leaf Stomatal Conductance", ncdims=ncdims)
      output <- var_update(sub.maat.output$ci, output, "ci", "Ci", oldunits="Pa", 
                           newunits="Pa", missval=-999, longname="Leaf Internal CO2 Concentration", ncdims=ncdims)
      output <- var_update(sub.maat.output$cc, output, "cc", "Cc", oldunits="Pa", 
                           newunits="Pa", missval=-999, longname="Leaf Mesophyll CO2 Concentration", ncdims=ncdims)
      ## !!TODO: ADD MORE MAAT OUTPUTS HERE!! ##

    } else {
      t <- ncdf4::ncdim_def(name = "time", units = paste0("days since ", maat_run_start_date),
                            vals = 1, calendar = "standard", 
                            unlim = TRUE)  # standard calendar for leap years?  Also need to be sure we update cal depending on leap/no leap
      output <- NULL
      ncdims <- list(lon, lat, t) 
      output <- var_update(maat.output$A, output, "A", "GPP", oldunits="umol C m-2 s-1", newunits="kg C m-2 s-1", missval=-999,
                           longname="Gross Primary Productivity", ncdims=ncdims)
      output <- var_update(maat.output$rd, output, "rd", "leaf_respiration", oldunits="umol C m-2 s-1", newunits="kg C m-2 s-1", missval=-999,
                           longname="Leaf Respiration Rate", ncdims=ncdims)
      output <- var_update((1/(maat.output$rs)), output, "gs", "stomatal_conductance", oldunits="mol H2O m-2 s-1",
                           newunits="kg H2O m-2 s-1", missval=-999, longname="Leaf Stomatal Conductance", ncdims=ncdims)
      output <- var_update(maat.output$ci, output, "ci", "Ci", oldunits="Pa",
                           newunits="Pa", missval=-999, longname="Leaf Internal CO2 Concentration", ncdims=ncdims)
      output <- var_update(maat.output$cc, output, "cc", "Cc", oldunits="Pa",
                           newunits="Pa", missval=-999, longname="Leaf Mesophyll CO2 Concentration", ncdims=ncdims)
      ## !!TODO: ADD MORE MAAT OUTPUTS HERE!! ##  
    }
    
    ## write netCDF data
    ncout <- ncdf4::nc_create(file.path(outdir, paste(year, "nc", sep = ".")),output$var)
    for (i in seq_along(output$var)) {
      ncdf4::ncvar_put(ncout, output$var[[i]], output$dat[[i]])
    }
    
    ## extract variable and long names to VAR file for PEcAn vis
    utils::write.table(sapply(ncout$var, function(x) { x$longname }), 
                file = file.path(outdir, paste(year, "nc.var", sep = ".")), 
                col.names = FALSE, 
                row.names = TRUE, 
                quote = FALSE)
    
    # close netCDF file
    try(ncdf4::nc_close(ncout))
    
  }  ## Year loop
} # model2netcdf.MAAT
##-------------------------------------------------------------------------------------------------#
## EOF
