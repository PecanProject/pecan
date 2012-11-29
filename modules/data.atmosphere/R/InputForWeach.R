##' Download Met Drivers for BioCro from NCEP
##'
##' @title InputForWeach 
##' @param lat numeric latitude
##' @param lon numeric longitude
##' @param year1 integer
##' @param year2 integer
##' @return climate data to be parsed by the \code{\link{weachNEW}} function in BioCro
##' @author Deepak Jaiswal, David LeBauer
InputForWeach <- function(lat, lon, year1, year2){
  ncep.inputs <- list(year1 = year1, year2 = year2, lat = lat, lon = lon)
  ## Get Temperature Records
  avgTemp <- ncep.gather2(variable = "air.2m", inputs = ncep.inputs)
  avgTemp <- NCEP.aggregate(avgTemp, HOURS = FALSE, fxn = "mean")  # Average Flux for the whole day
  avgTemp <- NCEP.array2df(avgTemp, var.name = "avgTemp")
  avgTemp <- aggregate(avgTemp ~ datetime, data = avgTemp, mean)  # mean of all nearby spatial locations
  avgTemp$datetime <- substr(avgTemp$datetime, 1, 10)
  avgTemp$avgTemp <- avgTemp$avgTemp - 273
  
  ## Get Solar Radiation Records
  solarR <- ncep.gather2(variable = "dswrf.sfc", inputs = ncep.inputs)
  solarR <- NCEP.aggregate(solarR, HOURS = FALSE, fxn = "mean")  # Average Flux for the whole day
  solarR <- NCEP.array2df(solarR, var.name = "solarR")
  solarR$solarR <- solarR$solarR * 24 * 60 * 60 * 1e-06  # To convert Wt/m2 to MJ/m2
  solarR <- aggregate(solarR ~ datetime, data = solarR, mean)  # mean of all nearby spatial locations
  solarR$datetime <- substr(solarR$datetime, 1, 10)
  
### T Maximum Data
  Tmax <- ncep.gather2(variable = "tmax.2m", inputs = ncep.inputs)
  Tmax <- NCEP.aggregate(Tmax, HOURS = FALSE, fxn = "max")
  Tmax <- NCEP.array2df(Tmax, var.name = "Tmax")
  Tmax <- aggregate(Tmax ~ datetime, data = Tmax, max)
  Tmax$datetime <- substr(Tmax$datetime, 1, 10)
  Tmax$Tmax <- Tmax$Tmax - 273
  
  
  ## T Minimum Data
  Tmin <- ncep.gather2(variable = "tmin.2m", inputs = ncep.inputs)
  Tmin <- NCEP.aggregate(Tmin, HOURS = FALSE, fxn = "max")
  Tmin <- NCEP.array2df(Tmin, var.name = "Tmin")
  Tmin <- aggregate(Tmin ~ datetime, data = Tmin, max)
  Tmin$datetime <- substr(Tmin$datetime, 1, 10)
  Tmin$Tmin <- Tmin$Tmin - 273
  
  ## Relative Humidity (I am using surface level, not Grid level to get rlative
  ## humidity, not absolute humidity, hope its not a problem.
  RH <- ncep.gather2(variable = c("rhum.sig995"), level = "surface", inputs = ncep.inputs)
  
  ## Warnign Message, not available in Reanalysis 2, Instead using Reanalysis 1.
  
  RHavg <- NCEP.aggregate(RH, HOURS = FALSE, fxn = "mean")
  RHmax <- NCEP.aggregate(RH, HOURS = FALSE, fxn = "max")
  RHmin <- NCEP.aggregate(RH, HOURS = FALSE, fxn = "min")
  RHavg <- NCEP.array2df(RHavg, var.name = "RH")
  RHmax <- NCEP.array2df(RHmax, var.name = "RH")
  RHmin <- NCEP.array2df(RHmin, var.name = "RH")
  
  RHavg <- aggregate(RH ~ datetime, data = RHavg, mean)
  RHmax <- aggregate(RH ~ datetime, data = RHmax, max)
  RHmin <- aggregate(RH ~ datetime, data = RHmin, min)
  RHavg$datetime <- substr(RHavg$datetime, 1, 10)
  RHmax$datetime <- substr(RHmax$datetime, 1, 10)
  RHmin$datetime <- substr(RHmin$datetime, 1, 10)
  RHavg$RH <- RHavg$RH * 0.01  ## Percent to Fraction
  RHmax$RH <- RHmax$RH * 0.01  ## Percent to Fraction
  RHmin$RH <- RHmin$RH * 0.01  ## Percent to Fraction
  
  
  ## Wind Speed
  
  Vwind <- ncep.gather2(variable = "vwnd.10m", inputs = ncep.inputs)
  Vwind <- NCEP.aggregate(Vwind, HOURS = FALSE, fxn = "mean")
  Vwind <- NCEP.array2df(Vwind, var.name = "Vwind")
  Vwind <- aggregate(Vwind ~ datetime, data = Vwind, mean)
  Vwind$datetime <- substr(Vwind$datetime, 1, 10)
  
  Uwind <- ncep.gather2(variable = "uwnd.10m", inputs = ncep.inputs)
  Uwind <- NCEP.aggregate(Uwind, HOURS = FALSE, fxn = "mean")
  Uwind <- NCEP.array2df(Uwind, var.name = "Uwind")
  Uwind <- aggregate(Uwind ~ datetime, data = Uwind, mean)
  Uwind$datetime <- substr(Uwind$datetime, 1, 10)
  
  Uwind$Uwind <- sqrt(Uwind$Uwind^2 + Vwind$Vwind^2)
  
  ## converting Windspeed from 10m to 2 m height using correlation
  ## provided by FAO (http://www.fao.org/docrep/X0490E/x0490e07.htm)
  Uwind$Uwind <- Uwind$Uwind * 4.87/log(67.8 * 10 - 5.42)  
  
  Uwind$Uwind <- Uwind$Uwind * (3600)/(1609)  # unit conversion from m/s to miles per hr
  
  
  ## Precipitation
  
  Rain <- ncep.gather2(variable = "prate.sfc", inputs = ncep.inputs)
  Rain <- NCEP.aggregate(Rain, HOURS = FALSE, fxn = "mean")
  Rain <- NCEP.array2df(Rain, var.name = "Rain")
  Rain <- aggregate(Rain ~ datetime, data = Rain, mean)
  Rain$datetime <- substr(Rain$datetime, 1, 10)
  Rain$Rain <- Rain$Rain * (24 * 60 * 60) * (1/1000) * 39.37  # Converting from kg/m2 sec to kg/m2 to m3/m2 to inches
  
  day <- numeric(0)
  year <- numeric(0)
  for (i in year1:year2){
    if ((i%%400) == 0 || (i%%100 != 0 && i%%4 == 0)){
      indx <- as.integer(length(day))
      day[as.integer(indx + 1):as.integer(indx + 366)] <- seq(1:366)
      year[as.integer(indx + 1):as.integer(indx + 366)] <- rep(i, 366)
    }
    if (!((i%%400) == 0 || (i%%100 != 0 && i%%4 == 0))){
      indx <- as.integer(length(day))
      day[as.integer(indx + 1):as.integer(indx + 365)] <- seq(1:365)
      year[as.integer(indx + 1):as.integer(indx + 365)] <- rep(i, 365)
    }
  }
  result <- data.frame(year = year, day = day, solarR = solarR$solarR, Tmax = Tmax$Tmax, 
                       Tmin = Tmin$Tmin, Tavg = avgTemp$avgTemp, RHmax = RHmax$RH, RHmin = RHmin$RH, 
                       RHavg = RHavg$RH, WS = Uwind$Uwind, precip = Rain$Rain)
  return(result)
}
##' Wrapper for \code{\link{RNCEP::NCEP.gather}} function to specify custom defaults
##'
##' @title ncep.gather2 
##' @param variable variable to be extracted from NCEP data (see ?NCEP.gather)
##' @param level passed to NCEP.gather, either "gaussian" (default), "surface" or numeric value of pressure 
##' @param inputs list of parameters passed to InputForWeach
##' @return data from NCEP
##' @author David LeBauer
##' @examples 
##' Uwind <- ncep.gather2(variable = "uwnd.10m", inputs = list(lat = 40, lon = 40, start.year = 2000, end.year = 2001))
ncep.gather2 <- function(variable, level = "gaussian", inputs) {
  result <- NCEP.gather(variable = variable,
                        level = level,
                        months.minmax = c(1, 12),
                        years.minmax = c(inputs$year1, inputs$year2),
                        lat.southnorth = c(inputs$lat, inputs$lat),
                        lon.westeast = c(inputs$lon, inputs$lon),
                        reanalysis2 = TRUE,
                        return.units = FALSE,
                        status.bar = FALSE)
  return(result)
}
