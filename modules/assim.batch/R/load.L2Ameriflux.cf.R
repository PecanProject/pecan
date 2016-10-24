##' Load Ameriflux L2 Data From NetCDF
##'
##' @title Load Ameriflux L2 Data From NetCDF
##' @name load.pda.data
##' @param file.in = the netcdf file of L2 data
##'
##' @return A data frame of all variables in the netcdf
##'
##' @author Ryan Kelly
##' @export
load.L2Ameriflux.cf <- function(file.in) {
  
  nc <- ncdf4::nc_open(file.in)
  
  vars <- list()
  for (varname in names(nc$var)) {
    vars[[varname]] <- ncdf4::ncvar_get(nc, varname)
  }
  ncdf4::nc_close(nc)
  
  return(as.data.frame(do.call(cbind, vars)))
} # load.L2Ameriflux.cf
