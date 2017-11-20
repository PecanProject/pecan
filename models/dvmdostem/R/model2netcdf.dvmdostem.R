#-------------------------------------------------------------------------------
# Copyright (c) 2016 NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

#outdir <- "/data/sserbin/Modeling/dvmdostem/pecan_runs/run.77/out/ENS-00001"

## helper function
#oldname <- "GPP"
#newname <- "GPP"
#oldunits <- "gC m-2 yr-1"
#newunits <- "kgC m-2 s-1"
#dims <-   out_nc_dims
#        var_update("AR","AutoResp","kgC m-2 s-1")
var_update <- function(out, ncin, dims, pixel, oldname, newname, oldunits=NULL, newunits=NULL){

  ## define variable
  if (is.null(oldunits)) oldunits <- ncdf4::ncatt_get(ncin,oldname,"units")$value
  # dvm-dos-tem needs updates to units metadata to support above, needs actual time (e.g. month, year) not just / time
  if(is.null(newunits)) newunits = oldunits
  newvar <- ncdf4::ncvar_def(name = newname, units = newunits, dim = dims)

  ## convert data
  dat <- ncdf4::ncvar_get(ncin,oldname)[pixel[1],pixel[2],]
  dat.new <- PEcAn.utils::misc.convert(dat,oldunits,newunits)

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
model2netcdf.dvmdostem <- function(outdir) {

  PEcAn.logger::logger.info(paste("Processing dvmdostem outputs in: ", outdir))

  # First things first, we need to check the run_status.nc file and make sure
  # that the a) only one pixel ran, and b) the success code is > 0
  nc_runstatus <- ncdf4::nc_open(file.path(outdir, "run_status.nc"), write=FALSE)
  run_status <- ncdf4::ncvar_get(nc_runstatus, nc_runstatus$var$run_status)
  ncdf4::nc_close(nc_runstatus)
  good_px <- which(run_status > 0)
  if (length(good_px) != 1) {
    PEcAn.logger::logger.error("ERROR! Either more than one pixel ran, or no pixel successfully ran. Not sure what to do, so quitting.")
    stop()
    # Not sure we even need to check bad_px or skipped_px?
    #skipped_px <- which(run_status == 0)
    #bad_px <- which(run_status < 0)
  }

  # Next check might be to look at the <run><start.date> and end date
  # and check that there is enough info in the output files to accomodate
  # the requested date range. We can do this later, as for now we'll assume it
  # is going to be ok.

  # Get the actual pixel coords of the cell that ran
  px <- which(run_status > 0, arr.ind = TRUE) # Returns x,y array indices
  px_X <- px[1]
  px_Y <- px[2]

  # So dvmdostem output files are per-variable, and contain a time series.
  # There is one file per stage (pr, eq, sp, tr, sc). Pecan output files have
  # one years worth of data for many variables. The time dimension will have
  # units of "days since <yr>-01-01 00:00:00" and the file will be named <yr>.nc

  dvmdostem_outputs <- c("GPP","NPP") # NOT SURE YET WHERE THIS LIST SHOULD BE SETUP??
  for (i in seq_along(1:length(dvmdostem_outputs)) ) {

    # Open the dvmdostem files for transient and scenario
    ncin_tr_y <- ncdf4::nc_open(file.path(outdir, paste0(dvmdostem_outputs[i],"_yearly_tr.nc")))
    ncin_sc_y <- ncdf4::nc_open(file.path(outdir, paste0(dvmdostem_outputs[i],"_yearly_sc.nc")))

    # Read in the time series of data for the pixel that ran
    tr_data <- ncdf4::ncvar_get(ncin_tr_y, dvmdostem_outputs[i])[px_X,px_Y,]
    sc_data <- ncdf4::ncvar_get(ncin_sc_y, dvmdostem_outputs[i])[px_X,px_Y,]

    # Get vector of dates for each timestep in the file
    tr_starts <- ncdf4.helpers::nc.get.time.series(ncin_tr_y)
    sc_starts <- ncdf4.helpers::nc.get.time.series(ncin_sc_y)

    # Sanity check (safety first!)
    stopifnot(length(sc_starts) == length(sc_data))
    stopifnot(length(tr_starts) == length(tr_data))

    # Look up the units in dvmdostem world
    original_units <- ncdf4::ncatt_get(ncin_tr_y, dvmdostem_outputs[i], "units")
    original_units <- as.character(unlist(original_units)[2]) # How to avoid hard coded index??
    #stopifnot(original_units == as.character(unlist(ncdf4::ncatt_get(ncin_sc_y, dvmdostem_outputs[i], "units"))[2])

    # This is a temporary hack, till dvm-dos-tem issue is resolved...
    # See issue #336 (https://github.com/ua-snap/dvm-dos-tem/issues/336)
    original_units <- gsub("time", "year", original_units)

    # Set units in PEcAn world
    if (dvmdostem_outputs[i] == 'GPP') {newunits <- "kgC m-2 s-1"}
    if (dvmdostem_outputs[i] == 'NPP') {newunits <- "kgC m-2 s-1"}

    # Convert the data
    tr_data_new <- PEcAn.utils::misc.convert(tr_data, original_units, newunits)
    sc_data_new <- PEcAn.utils::misc.convert(sc_data, original_units, newunits)

    # Tack everything together so we can loop over it all at once below
    all_starts <- c(tr_starts, sc_starts)
    all_data <- c(tr_data_new, sc_data_new)

    # Loop over all the time steps (yearly in this case), making one new
    # file for each timestep.
    for (j in seq_along(1:length(all_starts))) {

      # Create dimensions for new file
      lond <- ncdf4::ncdim_def(name='lon',
                               units="degrees_east",
                               vals=c(1), # <=== read from dvmdostem file!
                               longname="coordinate_longitude")

      latd <- ncdf4::ncdim_def(name='lat',
                               units="degrees_north",
                               vals=c(1), # <=== read from dvmdostem file!
                               longname="coordinate_latitude")

      timed <- ncdf4::ncdim_def(name='time',
                                units=paste0("days since ", format(all_starts[j], "%Y-%m-%d %H:%M:%S")),
                                vals=c(0),
                                unlim=TRUE,
                                longname="time",
                                calendar='365_day')

      out_nc_dims <- list(lond, latd, timed) # dimension order: X, Y, time

      yr <- format(all_starts[j], "%Y")

      newvar <- ncdf4::ncvar_def(dvmdostem_outputs[i], newunits, out_nc_dims)
      ncout <- ncdf4::nc_create(file.path(outdir, paste0(as.character(yr), ".nc")), newvar)
      ncdf4::ncvar_put(ncout, newvar, all_data[j], c(1,1,1), c(1,1,1))
      ncdf4::nc_close(ncout)
      print("SOMEWHERE TO STOP")

    } # end of loop over years

  } # end loop over all dvmdostem outputs

} # end of function
##-------------------------------------------------------------------------------------------------#
## EOF