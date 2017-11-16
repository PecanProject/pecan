#-------------------------------------------------------------------------------
# Copyright (c) 2016 NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

#outdir <- "/data/sserbin/Modeling/dvmdostem/pecan_runs/run.77/out/ENS-00001"


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

  # Get the actual pixel coords of the cell that ran
  px <- which(run_status > 0, arr.ind = TRUE) # Returns x,y array indices
  px_X <- px[1]
  px_Y <- px[2]
  ## helper function
  #oldname <- "GPP"
  #newname <- "GPP"
  #oldunits <- "gC m-2 yr-1"
  #newunits <- "kgC m-2 s-1"
  #dims <-   out_nc_dims
  #        var_update("AR","AutoResp","kgC m-2 s-1")
  var_update <- function(out, dims, pixel, oldname, newname, oldunits=NULL, newunits=NULL){

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

  # Define PEcAn style dimensions; reusable for many files
  transient_dims <- ncdf4::nc_open(file.path(outdir, "NPP_yearly_tr.nc"), write=FALSE)
  transient_time <- transient_dims$dim$time$units # however format is wrong
  # > transient_dims$dim$time$units
  # [1] "days since 1901-1-1 0:0:0" --> needs to be days since 1901-01-01 00:00:00
  try(ncdf4::nc_close(transient_dims))
  if (!any(is.na(file.info(file.path(outdir, "NPP_yearly_sc.nc"))))) {
    scenerio_dims <- ncdf4::nc_open(file.path(outdir, "NPP_yearly_sc.nc"), write=FALSE)
    scenerio_time <- scenerio_dims$dim$time$units # however format is wrong
  }
  #> scenerio_time
  #[1] "days since 2010-1-1 0:0:0" --> needs to be "days since 2010-01-01 00:00:00"
  try(ncdf4::nc_close(scenerio_dims))
  
  
  #!! update below to generate these for both transient and scenerio!!
  lond <- ncdf4::ncdim_def(name='lon',
                           units="degrees_east",
                           vals=c(1), # <=== read from dvmdostem file!
                           longname="coordinate_longitude")

  latd <- ncdf4::ncdim_def(name='lat', 
                           units="degrees_north",
                           vals=c(1), # <=== read from dvmdostem file! 
                           longname="coordinate_latitude")

  # Odd - not sure what PEcAn uses this for as there should be one
  # file for each year. Maybe monthly or daily resolution outputs?
  timed <- ncdf4::ncdim_def(name='time',
                            units=transient_time,
                            vals=c(0),
                            unlim=TRUE,
                            longname="time",
                            calendar='365_day')
  #xyt <- list(lond, latd, timed)
  out_nc_dims <- list(lond, latd, timed)
  
  ## Open a dvmdostem output files  
  ## !!! Will need a loop here to work on multiple types of dvm-dos-tem output (e.g. monthly / yearly, NPP, INPP, & other outputs)
  #dvmdostem_outputs <- c("GPP", "INGPP", "NPP", "INNPP")
  #dvmdostem_outputs <- c("NPP")
  dvmdostem_outputs <- c("GPP","NPP")  # need to make this more flexible to support more outputs, or user selected outputs
  #output      <- list()  # create empty output
  output <- NULL
  
  for (i in seq_along(1:length(dvmdostem_outputs)) ) {
    PEcAn.logger::logger.info(paste("Converting dvm-dos-tem output:",dvmdostem_outputs[i]))
    
    if (dvmdostem_outputs[i] == "GPP") {
      ncin <- ncdf4::nc_open(file.path(outdir, paste0(dvmdostem_outputs[i],"_yearly_tr.nc")))
      PEcAn.logger::logger.info(paste0("Length of time dim: ", ncin$dim$time$len))
      output <- var_update(output, dims = out_nc_dims, pixel = c(px_X, px_Y), oldname=dvmdostem_outputs[i], newname="GPP",
                           oldunits="gC m-2 yr-1", newunits="kgC m-2 s-1")
      gpp_dim_time_val <- ncin$dim$time$val 
      # need to make this flexible so we aren't getting this from first var
      
      ncdf4::nc_close(ncin)
    } # GPP
    if (dvmdostem_outputs[i] == "NPP") {
      ncin <- ncdf4::nc_open(file.path(outdir, paste0(dvmdostem_outputs[i],"_yearly_tr.nc")))
      PEcAn.logger::logger.info(paste0("Length of time dim: ", ncin$dim$time$len))
      output <- var_update(output, dims = out_nc_dims, pixel = c(px_X, px_Y), oldname=dvmdostem_outputs[i], newname="NPP",
                           oldunits="gC m-2 yr-1", newunits="kgC m-2 s-1")
      ncdf4::nc_close(ncin)
    } #NPP
  }
  try(ncdf4::nc_close(ncin)) # just in case
  rm(i)

  ## Output PEcAn netCDF files - still a work in progress
  ctr <- 1
  # temporary work around. probably need to revise this in the future
  out_yr <- as.numeric( sub("\\D*(\\d+).*", "\\1", transient_time) ) # temporary work around. proba
  #> out_yr
  #[1] 1901
  
  ## !!! This  needs to be more flexible to handle monthly and annual ouputs
  ## !!! currently only supports annual
  transient_years <- seq(out_yr,1901+length(gpp_dim_time_val)-1, 1)
  for (yr in seq_along(transient_years)  ) { # replace with something more flexible
    PEcAn.logger::logger.info(paste0("Processing year: ",out_yr))
    # update time - this needs to be more elegant
    output$var[[1]]$dim[[3]]$units <- paste0("years since ", as.character(out_yr), "-01-01 00:00:00")
    ## write netCDF data
    ncout <- ncdf4::nc_create(file.path(outdir,paste0(as.character(out_yr),".nc")),output$var)
    for (i in seq_along(output$var)) {
      ncdf4::ncvar_put(ncout, output$var[[i]], output$dat[[i]][yr])
    }
    
    ## extract variable and long names to VAR file for PEcAn vis
    write.table(sapply(ncout$var, function(x) { x$longname }), 
                file = file.path(outdir,paste0(as.character(out_yr), ".nc.var")), 
                col.names = FALSE, 
                row.names = TRUE, 
                quote = FALSE)
    
    try(ncdf4::nc_close(ncout))
    ctr <- ctr + 1
    out_yr <- out_yr + 1
  }
    
} # end of function
##-------------------------------------------------------------------------------------------------#
## EOF