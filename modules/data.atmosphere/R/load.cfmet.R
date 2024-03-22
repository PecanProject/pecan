
##' Load met data from PEcAn formatted met driver
##'
##' subsets a PEcAn formatted met driver file and converts to a data.frame object
##'
##' @param met.nc object of class ncdf4 representing an open CF compliant, PEcAn standard netcdf file with met data
##' @param lat numeric value of latitude
##' @param lon numeric value of longitude
##' @param start.date format is 'YYYY-MM-DD'
##' @param end.date format is 'YYYY-MM-DD'
##' @return data frame of met data
##' @importFrom rlang .data
##' @importFrom dplyr %>%
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
  if (base.units != "days") {
    time.idx <- PEcAn.utils::ud_convert(time.idx, basetime.string, paste("days since ", base.date))
  }
  time.idx <- PEcAn.utils::ud_convert(time.idx, "days", "seconds")
  date <- as.POSIXct.numeric(round(time.idx), origin = base.date, tz = "UTC")

  all.dates <- data.frame(index = seq_along(time.idx), date = date)
  
  if (start.date + lubridate::days(1) < min(all.dates$date)) {
   PEcAn.logger::logger.severe("run start date", start.date, "before met data starts", min(all.dates$date))
  }
  if (end.date > max(all.dates$date)) {
   PEcAn.logger::logger.severe("run end date", end.date, "after met data ends", max(all.dates$date))
  }
  
  run.dates <- all.dates %>%
    dplyr::filter(.data$date >= start.date & .data$date <= end.date) %>%
    dplyr::mutate(
      doy = lubridate::yday(.data$date),
      year = lubridate::year(.data$date),
      month = lubridate::month(.data$date),
      day  = lubridate::day(.data$date),
      hour = lubridate::hour(.data$date) + lubridate::minute(.data$date) / 60)

  results <- list()


  ## pressure naming hack pending https://github.com/ebimodeling/model-drivers/issues/2
  standard_names <- append(as.character(PEcAn.utils::standard_vars$standard_name), "surface_pressure")
  variables <- as.character(standard_names[standard_names %in% 
                                             c("surface_pressure", attributes(met.nc$var)$names)])
  
  
  vars <- lapply(variables, function(x) get.ncvector(x, lati = lati, loni = loni, 
                                                     run.dates = run.dates, met.nc = met.nc))

  names(vars) <- gsub("surface_pressure", "air_pressure", variables)

  return(cbind(run.dates, vars[!sapply(vars, is.null)]))
} # load.cfmet
