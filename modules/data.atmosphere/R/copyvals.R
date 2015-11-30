##' Get meteorology variables from Ameriflux L2 netCDF files and convert to netCDF CF format
##'
##' @name copyvals
##' @title copyvals
##' @export
##' @param nc1
##' @param var1
##' @param nc2
##' @param var2
##' @param units2=NULL
##' @param missval=-6999.0
##' @param verbose=FALSE should ouput of function be extra verbose
##' 
##' @author Josh Mantooth, Mike Dietze, Elizabeth Cowdery, Ankur Desai
## helper function to copy variables and attributes from one
## nc file to another. This will do conversion of the variables
## as well as on the min/max values

copyvals <- function(nc1, var1, nc2, var2, dim2, units2=NA, conv=NULL, missval=-6999.0, verbose=FALSE) {
  vals <- ncvar_get(nc=nc1, varid=var1)
  vals[vals==-6999 | vals==-9999] <- NA
  if (!is.null(conv)) {
    vals <- lapply(vals, conv)
  }
  if (is.na(units2)) {
    units2 <- ncatt_get(nc=nc1, varid=var1, attname='units', verbose=verbose)$value
  }
  var <- ncvar_def(name=var2, units=units2, dim=dim2, missval=missval, verbose=verbose)
  nc2 <- ncvar_add(nc=nc2, v=var, verbose=verbose)
  ncvar_put(nc=nc2, varid=var2, vals=vals)
  
  # copy and convert attributes
  att <- ncatt_get(nc1, var1, 'long_name')
  if (att$hasatt) {
    val <- att$value
    ncatt_put(nc=nc2, varid=var2, attname='long_name', attval=val)
  }
  
  att <- ncatt_get(nc1, var1, 'valid_min')
  if (att$hasatt) {
    val <- ifelse(is.null(conv), att$value, conv(att$value))
    ncatt_put(nc=nc2, varid=var2, attname='valid_min', attval=val)
  }
  
  att <- ncatt_get(nc1, var1, 'valid_max')
  if (att$hasatt) {
    val <- ifelse(is.null(conv), att$value, conv(att$value))
    ncatt_put(nc=nc2, varid=var2, attname='valid_max', attval=val)
  }
  
  att <- ncatt_get(nc1, var1, 'comment')
  if (att$hasatt) {
    val <- sub(', -9999.* = missing value, -6999.* = unreported value', '', att$value)
    ncatt_put(nc=nc2, varid=var2, attname='comment', attval=val)
  }
}

getLatLon <- function(nc1) {
  loc <- ncatt_get(nc=nc1, varid=0, attname='site_location')
  if (loc$hasatt) {
    lat <- as.numeric(substr(loc$value,20,28))
    lon <- as.numeric(substr(loc$value,40,48))
    return(c(lat, lon))
  } else {
    lat <- ncatt_get(nc=nc1, varid=0, attname='geospatial_lat_min')
    lon <- ncatt_get(nc=nc1, varid=0, attname='geospatial_lon_min')
    if (lat$hasatt && lon$hasatt) {
      return(c(as.numeric(lat$value), as.numeric(lon$value)))
    }
  }
  logger.severe("Could not get site location for file.")
}

