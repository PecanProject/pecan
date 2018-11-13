#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------


##-------------------------------------------------------------------------------------------------#
##' Function to convert MAAT model output to standard netCDF format
##'
##' Converts all output contained in a folder to netCDF.
##'
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
##' run_dir <- "~/scratch/run/"
##' if (! file.exists(run_dir)) dir.create(run_dir,recursive=TRUE)
##' output_dir <- "~/scratch/out/"
##' if (! file.exists(output_dir)) dir.create(output_dir,recursive=TRUE)
##' met_xml <- system.file("leaf_user_met.xml",package="PEcAn.MAAT")
##' file.copy(from = met_xml, to = run_dir, overwrite = TRUE)
##' example_output_file <- system.file("out.csv",package="PEcAn.MAAT")
##' file.copy(from = example_output_file, to = output_dir, overwrite = TRUE)
##' PEcAn.MAAT::model2netcdf.MAAT(run_dir, output_dir, sitelat=39.9712, sitelon=-74.4346, 
##'                              start_date="2005/01/01", end_date="2005/12/31")
##' ncfile <- ncdf4::nc_open(file.path(output_dir,"2005.nc"), write = TRUE)
##' dat <- PEcAn.utils::misc.convert(ncdf4::ncvar_get(ncfile,"assimilation_rate"),"kg C m-2 s-1", "umol C m-2 s-1")
##' try(ncdf4::nc_close(ncfile))
##' x_axis <- seq(as.Date("2005/01/01"), as.Date("2005/12/31"), length.out=length(dat))
##' dev.new(width=14, height=8, unit="in")
##' plot(x_axis,dat,xlab="Time", ylab="Assimilation_Rate (umol/m2/s)",type="l")
##' dev.off()
##' }
##' 
##' @export
##' @author Shawn Serbin, Anthony Walker, Alexey Shiklomanov
##'
model2netcdf.MAAT <- function(rundir, outdir, sitelat = -999, sitelon = -999, start_date = NULL, end_date = NULL) {

  # setup constants
  day_secs <- udunits2::ud.convert(1, "day", "seconds")
  
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
    maat_run_start_date <- format(lubridate::as_datetime(maat.output$time, tz = timezone)[1], "%Y-%m-%d %H:%M:%S")
    maat_dates <- strptime(maat.output$time, format = "%Y-%m-%d", tz = timezone)  
  } else {
    maat_run_start_date <- format(lubridate::as_datetime(start_date, tz = timezone)[1], "%Y-%m-%d %H:%M:%S")
  }
  
  ### setup nc file lat/long
  lat <- ncdf4::ncdim_def("lat", "degrees_north", vals = as.numeric(sitelat), longname = "station_latitude")
  lon <- ncdf4::ncdim_def("lon", "degrees_east", vals = as.numeric(sitelon), longname = "station_longitude")
  
  ### Setup outputs for netCDF file in appropriate units
  for (year in seq(start_year, end_year)) {
    if (file.exists(file.path(outdir, paste(year, "nc", sep = "."))) ) {
      PEcAn.logger::logger.debug(paste("---- Output year", year, "already exists."))
      next  ## skip, model output already present.
    }
    
    PEcAn.logger::logger.info(paste("---- Processing MAAT output year:", year))

    sub.maat.output <- maat.output[lubridate::year(maat_dates) == year, ]
    sub.maat.dates <- lubridate::as_date(sub.maat.output$time)
    sub.maat.doy <- lubridate::yday(sub.maat.dates)
    sub.maat.output.dims <- dim(sub.maat.output)
    dims <- dim(subset(
      sub.maat.output,
      strptime(time, format = "%Y-%m-%d", tz=timezone) == 
        seq(strptime(sub.maat.dates[1], format = "%Y-%m-%d", tz=timezone), by = "days", length = 1)
    ))
    timestep.s <- day_secs / dims[1] # e.g. 1800 = 30 minute timesteps
    dayfrac <- 1 / dims[1]
    day.steps <- head(seq(0, 1, by = dayfrac), -1)

    process_tbl <- tibble::tribble(
      ~data, ~oldname, ~newname, ~oldunits, ~newunits, ~longname,
      list(sub.maat.output$A), "A", "assimilation_rate", "umol C m-2 s-1", "kg C m-2 s-1", "Leaf assimilation rate",
      list(sub.maat.output$rd), "rd", "leaf_respiration", "umol C m-2 s-1", "kg C m-2 s-1", "Leaf Respiration Rate",
      list(1/sub.maat.output$rs), "gs", "stomatal_conductance", "mol H2O m-2 s-1", "kg H2O m-2 s-1", "Leaf Stomatal Conductance",
      list(sub.maat.output$ci), "ci", "Ci", "Pa", "Pa", "Leaf Internal CO2 Concentration",
      list(sub.maat.output$cc), "cc", "Cc", "Pa", "Pa", "Leaf Mesophyll CO2 Concentration"
    )

    if (met_exists) {
      maat_run_start_by_year <- format(lubridate::as_datetime(sub.maat.dates, tz = timezone)[1], "%Y-%m-%d %H:%M:%S")
      tvals <- (sub.maat.doy - 1) + day.steps
      bounds <- array(data = NA_real_, dim = c(length(tvals), 2))
      bounds[,1] <- tvals
      bounds[,2] <- bounds[,1] + dayfrac
      t <- ncdf4::ncdim_def(name = "time", units = paste0("days since ", maat_run_start_by_year),
                            vals = tvals, calendar = "standard", 
                            unlim = TRUE)  # standard calendar for leap years?  Also need to be sure we update cal depending on leap/no leap
      out.year <- as.numeric(rep(year, sub.maat.output.dims[1]))
      met_tbl <- tibble::tribble(
        ~data, ~oldname, ~newname, ~oldunits, ~newunits, ~longname,
        list(out.year), "Year", "Year", "YYYY", "YYYY", "Simulation Year",
        list(tvals), "FracJulainDay", "FracJulianDay", "Frac DOY", "Frac DOY", "Fraction of Julian Date",
        )
      process_tbl <- rbind.data.frame(met_tbl, process_tbl)
    } else {
      bounds <- array(data = NA_real_, dim = c(1,2))
      bounds[,1] <- 0
      bounds[,2] <- 1
      t <- ncdf4::ncdim_def(name = "time", units = paste0("days since ", maat_run_start_date),
                            vals = 1, calendar = "standard", 
                            unlim = TRUE)  # standard calendar for leap years?  Also need to be sure we update cal depending on leap/no leap
    }

    ncdims <- list(lon, lat, t)

    ## Subset data for processing
    # setup netCDF time variable for year
    time_interval <- ncdf4::ncdim_def(name = "hist_interval", 
                                      longname = "history time interval endpoint dimensions",
                                      vals = 1:2, units="")

    #output <- purrr::pmap(process_maat_variable, process_tbl, missval = -999, ncdims = ncdims)
    output <- purrr::pmap(process_tbl, process_maat_variable, missval = -999, ncdims = ncdims)

    output <- c(output, list(list(
      var = ncdf4::ncvar_def(name = "time_bounds", units = "", 
                             longname = "history time interval endpoints", 
                             dim = list(time_interval, time = t), 
                             prec = "double"),
      dat = c(rbind(bounds[, 1], bounds[, 2]))
    )))
    
    ## write netCDF data
    nc_vars <- purrr::map(output, "var") # Extract var from each output
    ncout <- ncdf4::nc_create(file.path(outdir, paste(year, "nc", sep = ".")), nc_vars)
    on.exit(ncdf4::nc_close(ncout))
    ncdf4::ncatt_put(ncout, "time", "bounds", "time_bounds", prec=NA)
    for (output_var in output) {
      #print(i)  # for debugging
      ncdf4::ncvar_put(ncout, output_var$var, output_var$dat)
    }
    
    ## extract variable and long names to VAR file for PEcAn vis
    utils::write.table(
      sapply(ncout$var, function(x) { x$longname }), 
      file = file.path(outdir, paste(year, "nc.var", sep = ".")), 
      col.names = FALSE, 
      row.names = TRUE, 
      quote = FALSE
    )
    
  }  ## Year loop
} # model2netcdf.MAAT

#' Prepare single MAAT output variable for PEcAn standard
#'
#' @param data Data values, as vector, matrix, or array
#' @param oldname Old variable name (character)
#' @param newname Target variable name (character)
#' @param oldunits Old variable units (character). Passed into [PEcAn.utils::misc.convert].
#' @param newunits New variable units (character). Passed into [PEcAn.utils::misc.convert].
#' @param longname Long, descriptive variable name (character)
#' @param ncdims NetCDF dimensions (list). Passed into `dim` argument
#'   of [ncdf4::ncvar_def]
#' @param missval Value for missing data (numeric, default = -999)
#' @return List containing NetCDF variable (`var`) and converted data
#'   values (`dat`)
#' @author Alexey Shiklomanov
process_maat_variable <- function(data, oldname, newname, oldunits, newunits, longname, ncdims, missval = -999) {
  ## define function
  f_sort <- function(s) {
    if (is.infinite(s)) missval
    else try(PEcAn.utils::misc.convert(s, oldunits, newunits), silent = TRUE)
  }
  ## define variable
  if(is.null(newunits)) newunits = oldunits
  newvar <- ncdf4::ncvar_def(name = newname, units = newunits, dim = ncdims, missval = missval, longname = longname)
  if (newname %in% c("Year", "FracJulianDay")) {
    PEcAn.logger::logger.info(paste0("Skipping conversion for: ", newname))
    dat.new <- data
  } else {
    #dat.new <- PEcAn.utils::misc.convert(unlist(as.vector(data)), oldunits, newunits)
    #dat.new[!is.finite(dat.new)] <- missval
    
    dat.new <- apply(as.matrix(unlist(as.vector(data)),length(unlist(as.vector(data))),1),1,f_sort)
  }
  list(var = newvar, dat = dat.new)
}
## EOF
