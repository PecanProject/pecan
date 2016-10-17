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
load.cfmet <- cruncep_nc2dt <- function(met.nc, lat, lon, start.date, end.date) {
  
  library(lubridate)
  library(udunits2)
  library(data.table)
  library(PEcAn.utils)
  
  ## Lat and Lon
  Lat <- ncvar_get(met.nc, "latitude")
  Lon <- ncvar_get(met.nc, "longitude")
<<<<<<< HEAD
  
  if (min(abs(Lat - lat)) > 2.5 | min(abs(Lon - lon)) > 2.5) {
    logger.error("lat / lon (", lat, ",", lon, 
                 ") outside range of met file (", range(Lat), ",", range(Lon))
  }
  
=======

  if(min(abs(Lat-lat)) > 2.5 | min(abs(Lon-lon)) > 2.5){
    logger.error("lat / lon (", lat, ",", lon, ") outside range of met file (", range(Lat), ",", range(Lon))
  }
>>>>>>> PecanProject/master
  lati <- which.min(abs(Lat - lat))
  loni <- which.min(abs(Lon - lon))

  time.idx <- ncvar_get(met.nc, "time")

  ## confirm that time units are PEcAn standard
<<<<<<< HEAD
  basetime.string <- ncatt_get(met.nc, "time", "units")$value
  base.date       <- parse_date_time(basetime.string, c("ymd_hms", "ymd_h", "ymd"))
  base.units      <- strsplit(basetime.string, " since ")[[1]][1]
  
=======
  basetime.string <- ncatt_get(met.nc, 'time', 'units')$value
  base.date <- parse_date_time(basetime.string, c("ymd_hms", "ymd_h", "ymd"))

  base.units <- strsplit(basetime.string, " since ")[[1]][1]


>>>>>>> PecanProject/master
  ## convert to days
  if (!base.units == "days") {
    time.idx <- ud.convert(time.idx, basetime.string, paste("days since ", base.date))
  }
  time.idx <- ud.convert(time.idx, "days", "seconds")
  date <- as.POSIXct.numeric(time.idx, origin = base.date, tz = "UTC")
<<<<<<< HEAD
  
  ## data table warns not to use POSIXlt, which is induced by round() 
  ## but POSIXlt moves times off by a second
  suppressWarnings(all.dates <- data.table(index = seq(time.idx), date = round(date)))
  
  if (ymd(as.Date(start.date)) + days(1) < min(all.dates$date)) {
    logger.error("run start date", ymd(as.Date(start.date)), "before met data starts", min(all.dates$date))
  }
  if (ymd(as.Date(end.date)) > max(all.dates$date)) {
    logger.error("run end date", ymd(as.Date(start.date)), "after met data ends", min(all.dates$date))
  }
  
  run.dates <- all.dates[date > ymd(as.Date(start.date)) & date < ymd(as.Date(end.date)),
                         list(index, 
                              date = date, 
                              doy = yday(date),
                              year = year(date),
                              month = month(date),
                              day  = day(date), 
                              hour = hour(date) + minute(date) / 60)]
  
=======



  suppressWarnings(
    ## data table warns not to use POSIXlt, which is induced by round()
    ## but POSIXlt moves times off by a second
    all.dates <- data.table(index = seq(time.idx),
                            date = round(date))
  )



  if(ymd(start.date + days(1)) < as.Date(ymd_hms(min(all.dates$date)))) {
    logger.error("run start date", ymd(as.Date(start.date)),
                 "before met data starts", min(all.dates$date))
  }
  if(ymd(end.date) > as.Date(ymd_hms(max(all.dates$date)))) {
    logger.error("run end date",   ymd(as.Date(start.date)),
                 "after met data ends", min(all.dates$date))
  }

  run.dates <- all.dates[date > start.date & date < end.date,
                         list(index, date = date, doy = yday(date),
                              year = year(date), month = month(date),
                              day  = day(date), hour = hour(date) + minute(date)/60)]

>>>>>>> PecanProject/master
  results <- list()

  data(mstmip_vars, package = "PEcAn.utils")

  ## pressure naming hack pending https://github.com/ebimodeling/model-drivers/issues/2
  standard_names <- append(as.character(mstmip_vars$standard_name), "surface_pressure")
<<<<<<< HEAD
  variables <- as.character(standard_names[standard_names %in% 
                                             c("surface_pressure", attributes(met.nc$var)$names)])
  
  
  vars <- lapply(variables, function(x) get.ncvector(x, lati = lati, loni = loni, 
=======
  variables <- as.character(standard_names[standard_names %in% c("surface_pressure", attributes(met.nc$var)$names)])
  variables <- variables[!variables %in% c('latitude', 'longitude')]
  vars <- lapply(variables, function(x) get.ncvector(x, lati = lati, loni = loni,
>>>>>>> PecanProject/master
                                                     run.dates = run.dates, met.nc = met.nc))

  names(vars) <- gsub("surface_pressure", "air_pressure", variables)
<<<<<<< HEAD
  
  cbind(run.dates, as.data.table(vars[!sapply(vars, is.null)]))
} # load.cfmet
=======

  result <- cbind(run.dates, as.data.table(vars[!sapply(vars, is.null)]))

  return(result)
}

>>>>>>> PecanProject/master
