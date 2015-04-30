## ensures data.table objects treated as such
## http://stackoverflow.com/q/24501245/513006
.datatable.aware=TRUE

##' Load met data from PEcAn formatted met driver
##'
##' subsets a PEcAn formatted met driver file and converts to a data.table / data.frame object
##' @title load CF met
##' @param met.nc object of class ncdf4 representing an open CF compliant, PEcAn standard netcdf file with met data
##' @param lat numeric value of latutude
##' @param lon numeric value of longitude
##' @param start.date format is "YYYY-MM-DD"
##' @param end.date format is "YYYY-MM-DD"
##' @return data.table of met data
##' @export
##' @author David LeBauer
load.cfmet <- cruncep_nc2dt <- function(met.nc, lat, lon, start.date, end.date){
  ## Lat and Lon
  Lat <- ncvar_get(met.nc, "latitude")
  Lon <- ncvar_get(met.nc, "longitude")
  
  if(min(abs(Lat-lat)) > 2.5 | min(abs(Lon-lon)) > 2.5) logger.error("lat / lon (", lat, ",", lon, ") outside range of met file (", range(Lat), ",", range(Lon))
  
  lati <- which.min(abs(Lat - lat))
  loni <- which.min(abs(Lon - lon))
  
  time.idx <- ncvar_get(met.nc, "time")
  
  ## confirm that time units are PEcAn standard
  time.units <- unlist(strsplit(met.nc$dim$time$units, " since "))
  if(!grepl("days", time.units[1])) {
    logger.error("time dimension does not have units of days")
  }
  date <- ymd(time.units[2])
  if(is.na(date)) date <- ymd_h(time.units[2])
  if(is.na(date)) date <- ymd_hms(time.units[2])
  all.dates <- data.table(index = seq(time.idx),
                          date = date +
                            days(floor(time.idx)) +
                            minutes(as.integer(ud.convert(time.idx - floor(time.idx), "days", "minutes"))))
  
  
  if(ymd(start.date) + days(1) < min(all.dates$date)) logger.error("run start date", ymd(start.date), "before met data starts", min(all.dates$date))
  if(ymd(end.date) > max(all.dates$date)) logger.error("run end date",   ymd(start.date), "after met data ends", min(all.dates$date))
  
  run.dates <- all.dates[date > ymd(start.date) & date < ymd(end.date),
                         list(index, date, doy = yday(date),
                              year = year(date), month = month(date),
                              day  = day(date), hour = hour(date))]
  
  results <- list()
  
  data(mstmip_vars, package = "PEcAn.utils")
  
  ## pressure naming hack pending https://github.com/ebimodeling/model-drivers/issues/2
  standard_names <- append(as.character(mstmip_vars$standard_name), "surface_pressure")
  variables <- as.character(standard_names[standard_names %in% c("surface_pressure", attributes(met.nc$var)$names)])
  
  
  vars <- lapply(variables, function(x) get.ncvector(x, lati = lati, loni = loni, 
                                                     run.dates = run.dates, met.nc = met.nc))
  
  names(vars) <- gsub("surface_pressure", "air_pressure", variables)
  
  result <- cbind(run.dates, as.data.table(vars[!sapply(vars, is.null)]))
  
  return(result)
}

