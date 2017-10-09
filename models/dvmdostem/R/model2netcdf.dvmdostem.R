#-------------------------------------------------------------------------------
# Copyright (c) 2016 NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

#outdir <- "/data/sserbin/Modeling/dvmdostem/pecan_runs/test_dir"


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
  
  PEcAn.logger::logger.info(paste("DVM-DOS-TEM outputs in:",outdir))
  
  ## helper function
  #oldname <- "GPP"
  #newname <- "GPP"
  #oldunits <- "gC m-2 yr-1"
  #newunits <- "kgC m-2 s-1"
  #dims <-   out_nc_dims
  #        var_update("AR","AutoResp","kgC m-2 s-1")
  var_update <- function(out, dims, pixel = c(10,10), oldname, newname, oldunits=NULL, newunits=NULL){
    
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

  ## Setup for output
  # Define PEcAn style dimensions
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
                            units='years since 1901-01-01 00:00:00',
                            vals=c(0),
                            unlim=TRUE,
                            longname="time",
                            calendar='standard')
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
      output <- var_update(output, dims = out_nc_dims, oldname=dvmdostem_outputs[i], newname="GPP", 
                           oldunits="gC m-2 yr-1", newunits="kgC m-2 s-1")
      gpp_dim_time_val <- ncin$dim$time$val 
      # need to make this flexible so we aren't getting this from first var
      
      ncdf4::nc_close(ncin)
    } # GPP
    if (dvmdostem_outputs[i] == "NPP") {
      ncin <- ncdf4::nc_open(file.path(outdir, paste0(dvmdostem_outputs[i],"_yearly_tr.nc")))
      PEcAn.logger::logger.info(paste0("Length of time dim: ", ncin$dim$time$len))
      output <- var_update(output, dims = out_nc_dims, oldname=dvmdostem_outputs[i], newname="NPP", 
                           oldunits="gC m-2 yr-1", newunits="kgC m-2 s-1")
      ncdf4::nc_close(ncin)
    } #NPP
  }
  try(ncdf4::nc_close(ncin)) # just in case
  rm(i)

  ## Output PEcAn netCDF files - still a work in progress
  ctr <- 1
  out_yr <- 1901
  #out_yr <- 1902
  ## !!! This  needs to be more flexible to handle monthly and annual ouputs
  ## !!! currently only supports annual
  for (yr in gpp_dim_time_val) { # replace with something more flexible
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