##' Load from netCDF
##'
##' @param data.path character vector or list
##' @param format list
##' @param site list
##' @param vars character
##' @author Istem Fer
##' @export
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
    t.units <- ncdf4::ncatt_get(nc[[i]], dims[time.var])$units
    # If the unit has if of the form * since YYYY-MM-DD * with "-hour" timezone offset
    # This is a feature of the met produced by met2CF
    if(stringr::str_detect(t.units, "ince\\s[0-9]{4}[.-][0-9]{2}[.-][0-9]{2}.*\\s-\\d+")){
      unit2  <- stringr::str_split_fixed(t.units,"\\s-",2)[1]
      offset <- stringr::str_split_fixed(t.units,"\\s-",2)[2] %>% as.numeric()
      date_time <- suppressWarnings(try(lubridate::ymd((unit2))))
      if(is.na(date_time)){
        date_time <- suppressWarnings(try(lubridate::ymd_hms(unit2)))
      }
      if(is.na(date_time)){
        PEcAn.logger::logger.error("All time formats failed to parse. No formats found.")
      }
      t.units <- paste(stringr::str_split_fixed(t.units," since",2)[1], "since",
                       date_time - lubridate::hms(paste(offset,":00:00")))
    }else if(stringr::str_detect(t.units, "ince\\s[0-9]{4}[.-][0-9]{2}[.-][0-9]{2}.*")){
      unit2  <- stringr::str_split_fixed(t.units,"\\s-",2)[1]
      date_time <- suppressWarnings(try(lubridate::ymd((unit2))))
      if(is.na(date_time)){
        date_time <- suppressWarnings(try(lubridate::ymd_hms(unit2)))
      }
      if(is.na(date_time)){
        PEcAn.logger::logger.error("All time formats failed to parse. No formats found.")
      }
      t.units <- paste(stringr::str_split_fixed(t.units," since",2)[1], "since",
                       date_time)
    }
    # for heterogenous formats try parsing ymd_hms
    date.origin <- suppressWarnings(try(lubridate::ymd_hms(t.units)))
    # parsing ymd
    if (is.na(date.origin)) {
      date.origin <- lubridate::ymd(t.units)
    }
    # throw error if can't parse time format
    if (is.na(date.origin)) {
      PEcAn.logger::logger.error("All time formats failed to parse. No formats found.")
    }
    time.stamp.match <- gsub("UTC", "", date.origin)
    t.units <- gsub(paste0(" since ", time.stamp.match, ".*"), "", 
                    t.units)
    # need to change system TZ otherwise, lines below keeps writing in the current time zone
    Sys.setenv(TZ = 'UTC')
    foo <- as.POSIXct(date.origin, tz = "UTC") + PEcAn.utils::ud_convert(time.col[[i]], t.units, "seconds")
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
