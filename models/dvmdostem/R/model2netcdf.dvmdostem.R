#-------------------------------------------------------------------------------
# Copyright (c) 2016 NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
##-------------------------------------------------------------------------------------------------#
##' @name model2netcdf.dvmdostem
##' @title Code to convert dvmdostem netcdf output into into CF standard
##'
##' @param outdir Location of dvmdostem model output
##' 
##' @examples  
##' \dontrun{
##' # example code here?
##' }
##' 
##' @export
##'
##' @author Tobey Carman, Shawn Serbin
##'
model2netcdf.dvmdostem <- function(outdir, runstart, runend) {

  PEcAn.logger::logger.info(paste0("Run start: ", runstart, " Run end: ", runend))
  PEcAn.logger::logger.info(paste0("Processing dvmdostem outputs in: ", outdir))

  # First things first, we need to check the run_status.nc file and make sure
  # that the a) only one pixel ran, and b) the success code is > 0
  nc_runstatus <- ncdf4::nc_open(file.path(outdir, "run_status.nc"), write=FALSE)
  run_status <- ncdf4::ncvar_get(nc_runstatus, nc_runstatus$var$run_status)
  ncdf4::nc_close(nc_runstatus)
  good_px <- which(run_status > 0)
  if (length(good_px) != 1) {
    PEcAn.logger::logger.error("ERROR! Either more than one pixel ran, or no pixel successfully ran.")
    PEcAn.logger::logger.error("Not sure what to do, so quitting.")
    stop()
    # Not sure we even need to check bad_px or skipped_px?
    #skipped_px <- which(run_status == 0)
    #bad_px <- which(run_status < 0)
  }

  # Get the actual pixel coords of the cell that ran
  px <- which(run_status > 0, arr.ind = TRUE) # Returns x,y array indices
  px_X <- px[1]
  px_Y <- px[2]
  PEcAn.logger::logger.info(paste0("Using pixel ", px_X, ", ", px_Y))

  PEcAn.logger::logger.info("Done checking pixel.")

  # Next check might be to look at the <run><start.date> and end date
  # and check that there is enough info in the output files to accomodate
  # the requested date range. We can do this later, as for now we'll assume it
  # is going to be ok.

  # So dvmdostem output files are per-variable, and contain a time series.
  # There is one file per stage (pr, eq, sp, tr, sc). Pecan output files have
  # one years worth of data for many variables. The time dimension will have
  # units of "days since <yr>-01-01 00:00:00" and the file will be named <yr>.nc

  # psuedo code:
  # [x] open one of the dvmdostem yearly files, figure out how many years there are (both tr and sc)
  # [x] check that transient and scenario runs were contiguous in time
  # [ ]   - also maybe look at incoming args that control number of years to process
  #
  # [x] for each year in tr + sc:
  # [x]   create dimensions in pecan format
  # [x]   create variables list in pecan format
  # [x]   create new pecan style <year>.nc file with new vars
  # [x]   create .var file (something for pecan plotting?)
  # [x]   close new file
  #
  # REPEAT THIS BLOCK FOR SCENARIO
  # [x] for each transient year
  # [x]   open the appropriate <year>.nc file
  # [x]   for each variable:
  # [x]     open the dvmdostem variable file
  # [x]     pull out the data
  # [x]     handle unit (and name?) conversions
  # [x]     write the datapoint to the file
  # [x]   close the file

  dvmdostem_outputs <- c("GPP","NPP") # NOT SURE YET WHERE THIS LIST SHOULD BE SETUP??

  PEcAn.logger::logger.info(paste0("Opening dvmdostem raw output file for variable (transient): ", dvmdostem_outputs[1]))
  ncin_y_tr <- ncdf4::nc_open(file.path(outdir, paste0(dvmdostem_outputs[1],"_yearly_tr.nc")))
  y_tr_time_start <- ncin_y_tr$dim$time$units
  y_tr_time_start <- as.numeric( sub("\\D*(\\d+).*", "\\1", y_tr_time_start) )
  y_tr_starts <- paste0(seq(y_tr_time_start, y_tr_time_start + ncin_y_tr$dim$time$len-1, 1), "-01-01 00:00:00")

  PEcAn.logger::logger.info(paste0("Opening dvmdostem raw output file for variable (scenario): ", dvmdostem_outputs[1]))
  ncin_y_sc <- ncdf4::nc_open(file.path(outdir, paste0(dvmdostem_outputs[1],"_yearly_sc.nc")))
  y_sc_time_start <- ncin_y_sc$dim$time$units
  y_sc_time_start <- as.numeric( sub("\\D*(\\d+).*", "\\1", y_sc_time_start) )
  y_sc_starts <- paste0(seq(y_sc_time_start, y_sc_time_start + ncin_y_sc$dim$time$len-1, 1), "-01-01 00:00:00")

  # Check that transient and sceario runs were contiguous...
  if ((lubridate::year(y_tr_starts[length(y_tr_starts)]) + 1) != lubridate::year(y_sc_starts[1])) {
    PEcAn.logger::logger.error("WARNING! There is a gap between your transient and scenario datasets!!")
    PEcAn.logger::logger.error(paste0("End of transient:",
                                      lubridate::year(y_tr_starts[length(y_tr_starts)]),
                                      " Begining of scenario: ",
                                      lubridate::year(y_sc_starts[1])))
  }

  PEcAn.logger::logger.info("Creating one netcdf file for each output year...")
  all_yrs <- c(y_tr_starts, y_sc_starts)
  for (i in seq_along(1:length(all_yrs))) {

    PEcAn.logger::logger.info("Creating dimensions for new PEcAn style files...")
    lond <- ncdf4::ncdim_def(name='lon',
                             units="degrees_east",
                             vals=c(1), # <=== read from dvmdostem file!
                             longname="coordinate_longitude")

    latd <- ncdf4::ncdim_def(name='lat',
                             units="degrees_north",
                             vals=c(1), # <=== read from dvmdostem file!
                             longname="coordinate_latitude")

    timed <- ncdf4::ncdim_def(name='time',
                              units=paste0("days since ", all_yrs[i]),
                              vals=c(0),
                              unlim=TRUE,
                              longname="time",
                              calendar='365_day')

    out_nc_dims <- list(lon=lond, lat=latd, time=timed) # dimension order: X, Y, time

    PEcAn.logger::logger.info("Creating variables for new PEcAn style files...")
    newvars <- c() # Not very efficient, would be better to pre-allocate space
    for (j in seq_along(1:length(dvmdostem_outputs))) {
      # Use pecan utility function that can reognize and create proper longname
      # Need to handle name translation between dvmdostem names and pecan names...
      ncvar <- PEcAn.utils::to_ncvar(dvmdostem_outputs[j], out_nc_dims)

      # Alternatively could use this construct:
      # Set units in PEcAn world
      #if (dvmdostem_outputs[i] == 'GPP') {newname <- "GPP"; newunits <- "kgC m-2 s-1"; longname <- "Gross Primary Productivity"}
      #if (dvmdostem_outputs[i] == 'NPP') {newname <- "NPP"; newunits <- "kgC m-2 s-1"; longname <- "Net Primary Productivity"}
      #ncvar <- ncdf4::ncvar_def(name = newname , units = newunits, longname = longname, dim = out_nc_dims, -999, prec = "double")

      newvars[[j]] <- ncvar
    }

    ncout <- ncdf4::nc_create(file.path(outdir, paste0(as.character(lubridate::year(all_yrs[i])), ".nc")), newvars)
    # extract variable and long names to VAR file for PEcAn visibility
    # THIS NEEDS TO BE KEPT AND USED FOR PROPER PLOTTING
    write.table(sapply(ncout$var, function(x) { x$longname }),
                file = file.path(outdir,paste0(as.character(lubridate::year(all_yrs[i])), ".nc.var")),
                col.names = FALSE,
                row.names = TRUE,
                quote = FALSE)

    ncdf4::nc_close(ncout)
  }

  # Looping over the transient dvmdostem ouputs and writing data into the
  # the respective yearly PEcAn output files.
  for (i in seq_along(1:length(y_tr_starts))) {
    ncout <- ncdf4::nc_open(file.path(outdir, paste0(lubridate::year(y_tr_starts[i]), ".nc")), write = TRUE)
    for (j in dvmdostem_outputs){
      ncin_tr_y <- ncdf4::nc_open(file.path(outdir, paste0(j, "_yearly_tr.nc")))
      vardata <- ncdf4::ncvar_get(ncin_tr_y, j)

      # Look up the units in dvmdostem world
      original_units <- ncdf4::ncatt_get(ncin_tr_y, j, "units")
      original_units <- as.character(unlist(original_units)[2]) # How to avoid hard coded index??

      # This is a temporary hack, till dvm-dos-tem issue is resolved...
      # See issue #336 (https://github.com/ua-snap/dvm-dos-tem/issues/336)
      original_units <- gsub("time", "year", original_units)

      # Set units in PEcAn world
      if (j == 'GPP') {newunits <- "kgC m-2 s-1"}
      if (j == 'NPP') {newunits <- "kgC m-2 s-1"}

      # Convert the data
      vardata_new <- PEcAn.utils::misc.convert(vardata, original_units, newunits)

      # Write the data to the file...
      ncdf4::ncvar_put(ncout, j, vardata_new[px_X, px_Y,i], c(1,1,1), c(1,1,1))
    }
    ncdf4::nc_close(ncout)
  }

  # Looping over the scenario dvmdostem ouputs and writing data into the
  # the respective yearly PEcAn output files.
  for (i in seq_along(1:length(y_sc_starts))) {
    ncout <- ncdf4::nc_open(file.path(outdir, paste0(lubridate::year(y_sc_starts[i]), ".nc")), write = TRUE)
    for (j in dvmdostem_outputs){
      ncin_sc_y <- ncdf4::nc_open(file.path(outdir, paste0(j, "_yearly_sc.nc")))
      vardata <- ncdf4::ncvar_get(ncin_sc_y, j)

      # Look up the units in dvmdostem world
      original_units <- ncdf4::ncatt_get(ncin_sc_y, j, "units")
      original_units <- as.character(unlist(original_units)[2]) # How to avoid hard coded index??

      # This is a temporary hack, till dvm-dos-tem issue is resolved...
      # See issue #336 (https://github.com/ua-snap/dvm-dos-tem/issues/336)
      original_units <- gsub("time", "year", original_units)

      # Set units in PEcAn world
      if (j == 'GPP') {newunits <- "kgC m-2 s-1"}
      if (j == 'NPP') {newunits <- "kgC m-2 s-1"}

      # Convert the data
      vardata_new <- PEcAn.utils::misc.convert(vardata, original_units, newunits)

      # Write the data to the file...
      ncdf4::ncvar_put(ncout, j, vardata_new[px_X, px_Y,i], c(1,1,1), c(1,1,1))
    }
    ncdf4::nc_close(ncout)
  }

} # end of function
##-------------------------------------------------------------------------------------------------#
## EOF