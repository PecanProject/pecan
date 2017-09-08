#-------------------------------------------------------------------------------
# Copyright (c) 2016 NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

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
##' @importFrom ncdf4 ncdim_def ncvar_def ncatt_get ncvar_add
model2netcdf.dvmdostem <- function(outdir) {

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
                            units='years since 2000-01-01 00:00:00',
                            vals=c(0),
                            unlim=TRUE,
                            longname="time",
                            calendar='standard')
  
  # Define PEcAn style variables
  nppv <- ncdf4::ncvar_def(name="NPP",
                           units="kgC m-2 s-1",
                           dim=list(lond, latd, timed)) # WTF? dimension spec seems to be backwards...
  

  # Open a dvmdostem output file  
  d <- ncdf4::nc_open(file.path(outdir, "NPP_yearly_tr.nc"))
  #d = ncdf4::nc_open('NPP_yearly_tr.nc')
  a <- ncdf4::ncvar_get(d, 'NPP')
  nppvals <- a[10,10,]
  
  print(paste0("Length of time dim: ", d$dim$time$len))
  ctr <- 1
  #out_yr <- 1998
  out_yr <- 1901
  for (yr in d$dim$time$val) {


    # Write to file
    new_pecan_style_file = ncdf4::nc_create(file.path(outdir, paste0(out_yr, ".nc")), list(nppv))

    ncdf4::ncvar_put(nc=new_pecan_style_file, varid=nppv, vals=nppvals[ctr], verbose=TRUE)
    ctr <- ctr + 1
    out_yr <- out_yr + 1

    ncdf4::nc_close(new_pecan_style_file)

  }
    
}
