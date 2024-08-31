library(lubridate)

##-------------------------------------------------------------------------------------------------#
##' @title Write data into PEcAn shaped output file.
##' @param y_starts a list of years, i.e.: 1901, 1902, 1903, etc.
##' @param outdir a path to the location where were we will look for dvmdostem outputs and write PEcAn outputs.
##' @param pecan_requested_vars comma separated string listing the variables to process (PEcAn names).
##' @param monthly_dvmdostem_outputs list of files available from dvmdostem at monthly resolution.
##' @param yearly_dvmdostem_outputs list of files available from dvmdostem at yearly resolution.
##' @param px_Y the pixel offset, Y (latitude) dimension.
##' @param px_X the pixel offset, X (longitude) dimension.
##' @author Tobey Carman
##'
write.data2pecan.file <- function(y_starts, outdir, pecan_requested_vars, monthly_dvmdostem_outputs, yearly_dvmdostem_outputs, px_Y, px_X) {

  # Looping over the dvmdostem ouputs and writing data into the
  # the respective yearly PEcAn output files.
  for (i in seq_along(1:length(y_starts))) {
    ncout <- ncdf4::nc_open(file.path(outdir, paste0(lubridate::year(y_starts[i]), ".nc")), write = TRUE)
    for (j in pecan_requested_vars) {

      # Look up the depends_on in the reverse map
      for (k in vmap_reverse[[j]][["depends_on"]]) {
        # See that dvmdostem output files are available for each depends_on...
        print(paste0(j, " depends on ", k))
        print(paste0("Look for dvmdostem files for ", k))
      }

      # Make empty container for new data
      newVector <- vector(mode = "numeric")

      for (k in unlist(strsplit(vmap_reverse[[j]][["depends_on"]], ","))) {

        # Determine if dvmdostem file is monthly or yearly
        if (TRUE %in% sapply(monthly_dvmdostem_outputs, function(x) grepl(paste0("^",k,"_"), x))) {
          ncin_tr_y <- ncdf4::nc_open(file.path(outdir, paste0(k, "_monthly_tr.nc")))
        } else if (TRUE %in% sapply(yearly_dvmdostem_outputs, function(x) grepl(paste0("^",k,"_"), x))) {
          ncin_tr_y <- ncdf4::nc_open(file.path(outdir, paste0(k, "_yearly_tr.nc")))
        } else {
          PEcAn.logger::logger.error(paste0("ERROR!: ", k, " is not a monthly or yearly variable!"))
          stop()
        }

        # Get the data
        vardata <- ncdf4::ncvar_get(ncin_tr_y, k)

        # Look up the units in dvmdostem world
        original_units <- ncdf4::ncatt_get(ncin_tr_y, k, "units")
        original_units <- as.character(unlist(original_units)[2]) # How to avoid hard coded index??

        # This is a temporary hack, till dvm-dos-tem issue is resolved...
        # See issue #336 (https://github.com/ua-snap/dvm-dos-tem/issues/336)
        original_units <- gsub("time", "year", original_units)
        original_units <- gsub("gC", "g C", original_units)

        # Convert the data
        vardata_new <- PEcAn.utils::misc.convert(vardata, original_units, vmap_reverse[[j]][["newunits"]])

        # Coerce the data into the right shape (y, x, time).
        # With a single pixel run, the Y and X dimensions are lost when
        # reading from the file with ncdf4::ncvar_get, and the subsequent 
        # ncdf4::ncvar_put call fails. So here we make sure that the
        # vardata_new data is a 3D structure:
        dim_lengths <- sapply(ncin_tr_y$var[[1]]$dim, function(x) x$len)
        vardata_new <- array(vardata_new, dim = dim_lengths)

        dim.order <- sapply(ncin_tr_y$var[[k]]$dim, function(x) x$name)
        starts <-c(y = px_Y, x = px_X, time = 1)

        # What if variable as output from dvmdostem is by pft, or layer or pft and
        # compartment (pftpart)?
        #
        # Guessing/assuming that this is a "calibration run" and that the
        # dvmdostem calibraiton variables were enabled, which includes netCDF 
        # output by PFT and soil layer. These have to be sumarized in order to be 
        # included in the pecan output.") 
        #
        # Due to the way R handles NetCDF files, it appears that the dimensions of
        # vardata_new are (X, Y, PFT, TIME), even though in the NetCDf file, the 
        # dimensions are explicitly set as y, x, pft, time as reccomended by the
        # CF standards. In this case we want to sum over pft, which is the 3rd 
        # dimension in vardata_new. Note, to further confuse things, the 
        # ncdf4.helpers::nc.get.dim.names() function returns the following: 
        #
        #     Browse[4]> nc.get.dim.names(ncin_tr_y)
        #     [1] "time" "y"    "x"    "pft" 
        #
        # But some testing in an interactive R session seems to indicate that
        # the following apply function sums over PFTs as we want, and we end up
        # with vardata_new being an array with the dimensions X, Y, time
        if (length(dim(vardata_new)) == 5) {
          PEcAn.logger::logger.debug("")
          vardata_new <- apply(vardata_new, c(1,2,5), function(x) sum(x))
          dim.order <- dim.order[!dim.order %in% c('pft', 'pftpart')]
        }
        if (length(dim(vardata_new)) == 4){
          PEcAn.logger::logger.debug("")
          vardata_new <- apply(vardata_new, c(1,2,4), function(x) sum(x))
          dim.order <- dim.order[!dim.order %in% c('pft', 'layer')]
        }
        #if ('pft' %in% nc.get.dim.names(ncin_tr_y)) {}
        #if ('layers' %in% nc.get.dim.names(ncin_tr_y)) {}
        #if ('pft' %in% nc.get.dim.names(ncin_tr_y))

        if (TRUE %in% sapply(monthly_dvmdostem_outputs, function(x) grepl(paste0("^",k,"_"), x))) {
          # The current variable (j) is a monthly output
          counts <- c(y=1, x=1, time=12)
          startidx <- ((i-1)*12)+1
          endidx <- i*12
          newVector <- cbind(newVector, vardata_new[px_X, px_Y,startidx:endidx])
        } else if (TRUE %in% sapply(yearly_dvmdostem_outputs, function(x) grepl(paste0("^",k,"_"), x))) {
          # The current variable (k) is a yearly output
          counts <- c(y=1, x=1, time=1)
          newVector <- cbind(newVector, vardata_new[px_X, px_Y, i])
        } else {
          PEcAn.logger::logger.error(paste0("ERROR!: ", k, " is not a monthly or yearly variable!"))
          stop()
        }

      }

      # Maybe we will support more operations in the future besides sum...
      newVector <- apply(newVector, 1, sum)

      # Add new data to netcdf file...
      ncdf4::ncvar_put(ncout, j, newVector, start = starts[dim.order], count = counts[dim.order])
    }
    ncdf4::nc_close(ncout)
  }
}

##-------------------------------------------------------------------------------------------------#
##' @name model2netcdf.dvmdostem
##' @title Code to convert dvmdostem netcdf output into into CF standard
##' 
##' @param outdir Location of dvmdostem model output
##' @param runstart ??
##' @param runend ??
##' @param pecan_requested_vars a space separated string with names of the PEcAn variables to output.
##' @examples  
##' \dontrun{
##' # example code here?
##' }
##' 
##' @export
##'
##' @author Tobey Carman, Shawn Serbin
##'
model2netcdf.dvmdostem <- function(outdir, runstart, runend, pecan_requested_vars) {

  PEcAn.logger::logger.info(paste0("Run start: ", runstart, " Run end: ", runend))
  PEcAn.logger::logger.info(paste0("Processing dvmdostem outputs in: ", outdir))
  PEcAn.logger::logger.info(paste0("Building the following PEcAn variables: ", pecan_requested_vars))

  # Split apart the string of pecan vars passed into the function
  pecan_requested_vars <- unlist(lapply(unlist(strsplit(pecan_requested_vars, ",")), trimws))
  pecan_requested_vars <- unlist(lapply(pecan_requested_vars, function(x){x[!x==""]}))
  # Look up the required dvmdostem variables.
  dvmdostem_outputs <- ""
  for (pov in pecan_requested_vars) {
    dvmdostem_outputs <- trimws(paste(dvmdostem_outputs, vmap_reverse[[pov]][["depends_on"]], sep = ","))
  }
  dvmdostem_outputs <- unlist(lapply(unlist(strsplit(trimws(dvmdostem_outputs), ",")), function(x){x[!x== ""]}))

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

  # A less aggressive check here might be to see if enough of the transient
  # and scenario runs completed to do the analysis we need...


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

  monthly_dvmdostem_outputs <- list.files(outdir, "*_monthly_*")
  yearly_dvmdostem_outputs <- list.files(outdir, "*_yearly_*")

  # Look at the first dvmdostem output, see if it is was provided by dvmdostem
  # as monthly or yearly, and adjust accordingly.
  # NOTE: Assumes that all dvmdostem output files are at the same 
  # time resolution!
  if(TRUE %in% sapply(monthly_dvmdostem_outputs, function(x) grepl(paste0("^",dvmdostem_outputs[1],"_"), x))) {
    trfile <- file.path(outdir, paste0(dvmdostem_outputs[1], "_monthly_tr.nc"))
    scfile <- file.path(outdir, paste0(dvmdostem_outputs[1], "_monthly_sc.nc"))
    timedivisor <- 12
  } else if (TRUE %in% sapply(yearly_dvmdostem_outputs, function(x) grepl(paste0("^",dvmdostem_outputs[1],"_"), x))) {
    trfile <- file.path(outdir, paste0(dvmdostem_outputs[1], "_yearly_tr.nc"))
    scfile <- file.path(outdir, paste0(dvmdostem_outputs[1], "_yearly_sc.nc"))
    timedivisor <- 1
  } else {
    PEcAn.logger::logger.info(paste0("ERROR! - can't find ", dvmdostem_outputs[1], " in yearly or monthly outputs!?"))
    stop()
  }

  PEcAn.logger::logger.info(paste0("Opening dvmdostem raw output file for variable (transient): ", dvmdostem_outputs[1]))
  ncin_y_tr <- ncdf4::nc_open(trfile)
  y_tr_time_start <- ncin_y_tr$dim$time$units
  y_tr_time_start <- as.numeric( sub("\\D*(\\d+).*", "\\1", y_tr_time_start) )
  y_tr_time_end <- y_tr_time_start + (ncin_y_tr$dim$time$len/timedivisor) - 1
  y_tr_starts <- paste0(seq(y_tr_time_start, y_tr_time_end, 1), "-01-01 00:00:00")

  PEcAn.logger::logger.info(paste0("Opening dvmdostem raw output file for variable (scenario): ", dvmdostem_outputs[1]))
  ncin_y_sc <- ncdf4::nc_open(scfile)
  y_sc_time_start <- ncin_y_sc$dim$time$units
  y_sc_time_start <- as.numeric( sub("\\D*(\\d+).*", "\\1", y_sc_time_start) )
  y_sc_time_end <- y_sc_time_start + (ncin_y_sc$dim$time$len/timedivisor) - 1
  y_sc_starts <- paste0(seq(y_sc_time_start, y_sc_time_end, 1), "-01-01 00:00:00")


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

    PEcAn.logger::logger.info("Creating dimensions (and coordinate variables) for new PEcAn style files...")
    # The way R netcdf works is that you pass a vals argument when creating dimensions
    # and it creates the coordinate variables for you.
    lond <- ncdf4::ncdim_def(name='lon',
                             units="degrees_east",
                             vals=c(1), # <=== read from dvmdostem file! see dvmdostem issue #342
                             longname="coordinate_longitude")

    latd <- ncdf4::ncdim_def(name='lat',
                             units="degrees_north",
                             vals=c(1), # <=== read from dvmdostem file! see dvmdostem issue #342
                             longname="coordinate_latitude")

    if (length(monthly_dvmdostem_outputs) > 0) {
      # last day of each month
      timed_vals <- cumsum(sapply(seq(12), function(x) lubridate::days_in_month(x)))
    } else {
      timed_vals <- c(0)
    }
    timed <- ncdf4::ncdim_def(name='time',
                              units=paste0("days since ", all_yrs[i]),
                              vals=timed_vals,
                              unlim=TRUE,
                              longname="time",
                              calendar='365_day')

    out_nc_dims <- list(lon=lond, lat=latd, time=timed) # dimension order: X, Y, time

    PEcAn.logger::logger.info("Creating variables for new PEcAn style files...")

    newvars <- c() # Not very efficient, would be better to pre-allocate space
    j <- 0
    for (name in pecan_requested_vars){
      j <- j + 1
      print(paste0("Creating variable named: ", name))
      ncvar <- ncdf4::ncvar_def(name = name,
                                units = vmap_reverse[[name]][["newunits"]],
                                longname = vmap_reverse[[name]][["longname"]],
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

  # Write the transient data to pecan files.
  write.data2pecan.file(y_tr_starts, outdir, pecan_requested_vars, monthly_dvmdostem_outputs, yearly_dvmdostem_outputs, px_Y, px_X)

  # Write the scenario data to pecan files.
  write.data2pecan.file(y_sc_starts, outdir, pecan_requested_vars, monthly_dvmdostem_outputs, yearly_dvmdostem_outputs, px_Y, px_X)

} # end of function
##-------------------------------------------------------------------------------------------------#
## EOF
