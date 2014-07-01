
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
##' @author David LeBauer
load.cfmet <- cruncep_nc2dt <- function(met.nc, lat, lon, start.date, end.date){
  
  ## Lat and Lon
  Lat <- ncvar_get(met.nc, "lat")
  Lon <- ncvar_get(met.nc, "lon")
  
  lati <- which.min(abs(Lat - lat))
  loni <- which.min(abs(Lon - lon))
  
  time.idx <- ncvar_get(met.nc, "time")
  
  time.units <- unlist(strsplit(met.nc$dim$time$units, " since "))
  ref.date <- ymd_hms(time.units[2])
  if(grepl("hours", time.units[1])){
    time.idx <- ud.convert(time.idx, "hours", "days")
  }
  
  all.dates <- data.table(index = seq(time.idx),
                          date = ymd(ref.date) +
                            days(floor(time.idx)) +
                            minutes(ud.convert(time.idx - floor(time.idx), "days", "minutes")))
  run.dates <- all.dates[date > ymd(start.date) & date < ymd(end.date),
                         list(index, date, doy = yday(date),
                              year = year(date), month = month(date),
                              day  = day(date), hour = hour(date))]
  
  currentlat <- round(lat, 2)
  currentlon <- round(lon, 2)
  results <- list()
  

  
  #    variables <- c("lwdown", "press", "qair", "rain", "swdown", "tair", "northward_wind", "eastward_wind")

  variables <- attributes(met.nc$var)$names
  
  ## modification of ncvar_get to function independent of dimension order
  ## see http://stackoverflow.com/a/22944715/199217
  ## should be generalized, perhaps to pass arguments "start" and "count" directly
  
  vars <- lapply(variables, function(x) get.ncvector(x, lati = lati, loni = loni, run.dates = run.dates, met.nc = met.nc))
  
  names(vars) <- variables
  
  result <- cbind(run.dates, as.data.table(vars[!sapply(vars, is.null)]))
  if(!"wind" %in% variables){
    if(all(c("northward_wind", "eastward_wind") %in% variables)){
      result$wind <- result[,list(wind = sqrt(northward_wind^2 + eastward_wind^2))]
    }
  }
  return(result)   
}
