##' @name load_x_netcdf
##' @title load_x_netcdf
##' @export
##' @param data.path character vector or list
##' @param format list
##' @param start_year numeric
##' @param end_year numeric
##' @param site list
##' @param vars character
##' @author Istem Fer
load_x_netcdf <- function(data.path, format, site, vars = NULL) {
  
  data.path <- sapply(data.path, function(x) dir(dirname(x), basename(x), full.names = TRUE))
  
  nc <- lapply(data.path, ncdf4::nc_open)
  
  dat <- list()
  for (ind in seq_along(vars)) {
    nc.dat <- lapply(nc, ncdf4::ncvar_get, vars[ind])
    dat[vars[ind]] <- as.data.frame(unlist(nc.dat))
  }
  
  dat <- as.matrix(as.data.frame(dat))
  
  # we need to replace filling/missing values with NA now we don't want these values to go into unit
  # conversion
  dat[dat %in% as.numeric(format$na.strings)] <- NA
  dat <- as.data.frame(dat)
  colnames(dat) <- vars
  
  # deal with time
  time.col <- list()
  for (i in seq_along(nc)) {
    dims <- names(nc[[i]]$dim)
    time.var <- grep(pattern = "time", dims, ignore.case = TRUE)
    time.col[[i]] <- ncdf4::ncvar_get(nc[[i]], dims[time.var])

    # for heterogenous formats try parsing ymd_hms
    date.origin <- suppressWarnings(try(lubridate::ymd_hms(ncdf4::ncatt_get(nc[[i]], dims[time.var])$units)))
    
    # parsing ymd
    if (is.na(date.origin)) {
      date.origin <- lubridate::ymd(ncdf4::ncatt_get(nc[[i]], dims[time.var])$units)
    }
    # throw error if can't parse time format
    if (is.na(date.origin)) {
      PEcAn.logger::logger.error("All time formats failed to parse. No formats found.")
    }
    

    time.stamp.match <- gsub("UTC", "", date.origin)
    t.units <- gsub(paste0(" since ", time.stamp.match, ".*"), "", 
                    ncdf4::ncatt_get(nc[[i]], dims[time.var])$units)
    
    # need to change system TZ otherwise, lines below keeps writing in the current time zone
    Sys.setenv(TZ = 'UTC')
    foo <- as.POSIXct(date.origin, tz = "UTC") + udunits2::ud.convert(time.col[[i]], t.units, "seconds")
    time.col[[i]] <- foo
  }
  
  # needed to use 'round' to 'mins' here, otherwise I end up with values like '2006-12-31 23:29:59'
  # while reading Ameriflux for example however the model timesteps are more regular and the last
  # value can be '2006-12-31 23:30:00'..  this will result in cutting the last value in the
  # align_data step
  dat$posix <- round(as.POSIXct(do.call("c", time.col), tz = "UTC"), "mins")
  dat$posix <- as.POSIXct(dat$posix)
  
  lapply(nc, ncdf4::nc_close)
  
  return(dat)
} # load_x_netcdf
