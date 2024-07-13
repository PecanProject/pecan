##' @name pool_ic_netcdf2list
##' @title pool_ic_netcdf2list
##' @description Converts netcdf containing standard dimensions and variables for pool-based initial conditions, created by pool_ic_list2netcdf, back into list format
##' @export
##'
##' @param nc.path path to netcdf file containing standard dimensions and variables
##' @return list with two elements: list of netcdf dimensions (dims, with named values) and list of variables (vals, with named values)
##' @author Anne Thomas
pool_ic_netcdf2list <- function(nc.path){
  IC.nc <- try(ncdf4::nc_open(nc.path))
  on.exit(ncdf4::nc_close(IC.nc), add = FALSE) 
  if(!inherits(IC.nc, "try-error")) {
    dims <- vector(mode = "list", length = length(IC.nc$dim))
    names(dims) <- names(IC.nc$dim)
    for(i in seq(IC.nc$dim)){
      dims[[i]] <- IC.nc$dim[[i]]$vals
    }
    vals <- vector(mode = "list", length = length(IC.nc$var))
    names(vals) <- names(IC.nc$var)
    for(varname in names(vals)){
      vals[[varname]] <- ncdf4::ncvar_get(IC.nc,varname)
    }
    return(list(dims = dims, vals = vals))
  }
  else{
    PEcAn.logger::logger.severe("Could not read IC file.")
  }
  
}
