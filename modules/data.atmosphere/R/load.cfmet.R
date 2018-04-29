## ensures data.table objects treated as such http://stackoverflow.com/q/24501245/513006
.datatable.aware <- TRUE

##' Load met data from PEcAn formatted met driver
##'
##' subsets a PEcAn formatted met driver file and converts to a data.table / data.frame object
##' @title load CF met
##' @param met.nc object of class ncdf4 representing an open CF compliant, PEcAn standard netcdf file with met data
##' @param lat numeric value of latutude
##' @param lon numeric value of longitude
##' @param start.date format is 'YYYY-MM-DD'
##' @param end.date format is 'YYYY-MM-DD'
##' @return data.table of met data
##' @export
##' @author David LeBauer
load.cfmet <- function(met.nc, lat, lon, start.date, end.date) {
  
  ## Lat and Lon
  Lat <- ncdf4::ncvar_get(met.nc, "latitude")
  Lon <- ncdf4::ncvar_get(met.nc, "longitude")

  if(min(abs(Lat-lat)) > 2.5 | min(abs(Lon-lon)) > 2.5){
   PEcAn.logger::logger.severe("lat / lon (", lat, ",", lon, ") outside range of met file (", range(Lat), ",", range(Lon))
  }
  lati <- which.min(abs(Lat - lat))
  loni <- which.min(abs(Lon - lon))

  start.date <- lubridate::parse_date_time(start.date, tz = "UTC", orders=c("ymd_HMSz", "ymd_HMS", "ymd_H", "ymd"))

  # If end.date is provided as a datetime, assume it's the exact time to stop.
  # if it's a date, assume we want the whole final day.
  end.date <- tryCatch(
    lubridate::parse_date_time(end.date, tz = "UTC", orders = c("ymdHMSz", "ymdHMS")),
    warning = function(w){
      lubridate::parse_date_time(paste(end.date, "23:59:59"), tz = "UTC", orders = "ymdHMS")}
  )

  time.idx <- ncdf4::ncvar_get(met.nc, "time")

  ## confirm that time units are PEcAn standard
  basetime.string <- ncdf4::ncatt_get(met.nc, "time", "units")$value
  base.date       <- lubridate::parse_date_time(basetime.string, c("ymd_HMSz", "ymd_HMS", "ymd_H", "ymd"))
  base.units      <- strsplit(basetime.string, " since ")[[1]][1]

  ## convert to days
  if (!base.units == "days") {
    time.idx <- udunits2::ud.convert(time.idx, basetime.string, paste("days since ", base.date))
  }
  time.idx <- udunits2::ud.convert(time.idx, "days", "seconds")
  date <- as.POSIXct.numeric(time.idx, origin = base.date, tz = "UTC")

  ## data table warns not to use POSIXlt, which is induced by round() 
  ## but POSIXlt moves times off by a second
  suppressWarnings(all.dates <- data.table::data.table(index = seq(time.idx), date = round(date)))
  
  if (start.date + lubridate::days(1) < min(all.dates$date)) {
   PEcAn.logger::logger.error("run start date", start.date, "before met data starts", min(all.dates$date))
  }
  if (end.date > max(all.dates$date)) {
   PEcAn.logger::logger.error("run end date", end.date, "after met data ends", max(all.dates$date))
  }
  
  run.dates <- all.dates[date >= start.date & date <= end.date,
                         list(index, 
                              date = date, 
                              doy = lubridate::yday(date),
                              year = lubridate::year(date),
                              month = lubridate::month(date),
                              day  = lubridate::day(date), 
                              hour = lubridate::hour(date) + lubridate::minute(date) / 60)]
  
  results <- list()

  utils::data(mstmip_vars, package = "PEcAn.utils", envir = environment())

  ## pressure naming hack pending https://github.com/ebimodeling/model-drivers/issues/2
  standard_names <- append(as.character(mstmip_vars$standard_name), "surface_pressure")
  variables <- as.character(standard_names[standard_names %in% 
                                             c("surface_pressure", attributes(met.nc$var)$names)])
  
  
  vars <- lapply(variables, function(x) get.ncvector(x, lati = lati, loni = loni, 
                                                     run.dates = run.dates, met.nc = met.nc))

  names(vars) <- gsub("surface_pressure", "air_pressure", variables)

  return(cbind(run.dates, data.table::as.data.table(vars[!sapply(vars, is.null)])))
} # load.cfmet
