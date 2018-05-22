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
  if (length(nc_runstatus$var) != 1) {
    PEcAn.logger::logger.error(c("INVALID run_status.nc file! Expecting 1 variable, found: ", length(nc_runstatus$var)))
    stop()
  }

  run_status <- ncdf4::ncvar_get(nc_runstatus, nc_runstatus$var$run_status)

  # Cooerce the array into the right shape. the ncvar_get function does the 
  # right thing if we are reading run_mask with more than one pixel. But if
  # there is only one pixel, then a 1D list is returned, which causes problems
  # later in the function. So here we force the array into a 2D shape.
  dim.lengths = sapply(nc_runstatus$var[[1]]$dim, function(x) x$len)
  run_status <- array(run_status, dim=dim.lengths)

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

  dvmdostem_outputs <- c("GPP", "NPP", "RH", "SOC", "LAI") # NOT SURE YET WHERE THIS LIST SHOULD BE SETUP??

  # Build a mapping from dvmdostem names to PEcAn names, units, etc.
  # The temunits should (is) looked up from the dvmdostem output file's units
  # attributes...
  varmap <- list(
    "GPP"=c(newname="GPP", longname="Gross Primary Productivity", newunits="kg C m-2 s-1"),
    "NPP"=c(newname="NPP", longname="Net Primary Productivity", newunits="kg C m-2 s-1"),
    "RH"=c(newname="HeteroResp", longname="Heterotrophic Respiration", newunits="kg C m-2 s-1"),
    "SOC"=c(newname="SoilOrgC", longname="Soil Organic Carbon", newunits="kg C m-2"),
    "LAI"=c(newname="LAI", longname="Leaf Area Index", newunits="m-2/m-2")
  )

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
                             vals=c(1), # <=== read from dvmdostem file! see dvmdostem issue #342
                             longname="coordinate_longitude")

    latd <- ncdf4::ncdim_def(name='lat',
                             units="degrees_north",
                             vals=c(1), # <=== read from dvmdostem file! see dvmdostem issue #342
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
      # This pecan function doesn't always get the name translation correct
      # between PEcAn names and dvmdostem names, for example "RH" which in
      # dvmdostem world is "Heterotrophic Respiration", while
      # in pecan world, this gets interperted as "Relative Humidity".
      #ncvar <- PEcAn.utils::to_ncvar(dvmdostem_outputs[j], out_nc_dims)

      curvar <- varmap[[dvmdostem_outputs[j]]]

      ncvar <- ncdf4::ncvar_def(name = curvar[["newname"]],
                                units = curvar[["newunits"]],
                                longname = curvar[["longname"]],
                                dim = out_nc_dims, -999, prec = "double")

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
    for (j in dvmdostem_outputs) {
      ncin_tr_y <- ncdf4::nc_open(file.path(outdir, paste0(j, "_yearly_tr.nc")))
      vardata <- ncdf4::ncvar_get(ncin_tr_y, j)

      # Look up the units in dvmdostem world
      original_units <- ncdf4::ncatt_get(ncin_tr_y, j, "units")
      original_units <- as.character(unlist(original_units)[2]) # How to avoid hard coded index??

      # This is a temporary hack, till dvm-dos-tem issue is resolved...
      # See issue #336 (https://github.com/ua-snap/dvm-dos-tem/issues/336)
      original_units <- gsub("time", "year", original_units)
      original_units <- gsub("gC", "g C", original_units)

      # Set units in PEcAn world
      curvar <- varmap[[j]]

      # Convert the data
      vardata_new <- PEcAn.utils::misc.convert(vardata, original_units, curvar[["newunits"]])

      # Coerce the data into the right shape (y, x, time).
      # With a single pixel run, the Y and X dimensions are lost when
      # reading from the file with ncdf4::ncvar_get, and the subsequent 
      # ncdf4::ncvar_put call fails. So here we make sure that the
      # vardata_new data is a 3D structure:
      dim_lengths <- sapply(ncin_tr_y$var[[1]]$dim, function(x) x$len)
      vardata_new <- array(vardata_new, dim = dim_lengths)

      # Write the data to the file...
      starts <-c(y = px_Y, x = px_X, time = 1)
      counts <-c(y = 1, x = 1, time = 1)
      dim.order <- sapply(ncin_tr_y$var[[j]]$dim, function(x) x$name)
      ncdf4::ncvar_put(ncout, curvar[["newname"]], vardata_new[px_X, px_Y,i], start = starts[dim.order], count = counts[dim.order])
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
      original_units <- gsub("gC", "g C", original_units)

      # Set units in PEcAn world
      curvar <- varmap[[j]]

      # Convert the data
      vardata_new <- PEcAn.utils::misc.convert(vardata, original_units, curvar[["newunits"]])

      # Coerce the data into the right shape (y, x, time).
      # With a single pixel run, the Y and X dimensions are lost when
      # reading from the file with ncdf4::ncvar_get, and the subsequent 
      # ncdf4::ncvar_put call fails. So here we make sure that the
      # vardata_new data is a 3D structure:
      dim_lengths <- sapply(ncin_sc_y$var[[1]]$dim, function(x) x$len)
      vardata_new <- array(vardata_new, dim = dim_lengths)

      # Write the data to the file...
      starts <-c(y = px_Y, x = px_X, time = 1)
      counts <-c(y = 1, x = 1, time = 1)
      dim.order <- sapply(ncin_sc_y$var[[j]]$dim, function(x) x$name)
      ncdf4::ncvar_put(ncout, curvar[["newname"]], vardata_new[px_X, px_Y,i], start = starts[dim.order], count = counts[dim.order])

    }
    ncdf4::nc_close(ncout)
  }

} # end of function
##-------------------------------------------------------------------------------------------------#
## EOF