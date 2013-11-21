##' Download Met Drivers for BioCro from NCEP
##'
##' @title InputForWeach 
##' @param lat numeric latitude
##' @param lon numeric longitude
##' @param year1 integer
##' @param year2 integer
##' @export
##' @return climate data to be parsed by the \code{\link{weachNEW}} function in BioCro
##' @author Deepak Jaiswal, David LeBauer
InputForWeach <- function(lat, lon, year1, year2){
  ncep.inputs <- list(year1 = year1, year2 = year2, lat = lat, lon = lon)
  ## Get Temperature Records
  avgTemp <- ncep.gather2(variable = "air.2m", inputs = ncep.inputs)
  avgTemp <- RNCEP::NCEP.aggregate(avgTemp, HOURS = FALSE, fxn = "mean")  # Average Flux for the whole day
  avgTemp <- RNCEP::NCEP.array2df(avgTemp, var.name = "avgTemp")
  avgTemp <- aggregate(avgTemp ~ datetime, data = avgTemp, mean)  # mean of all nearby spatial locations
  avgTemp$datetime <- substr(avgTemp$datetime, 1, 10)
  avgTemp$avgTemp <- avgTemp$avgTemp - 273
  
  ## Get Solar Radiation Records
  solarR <- ncep.gather2(variable = "dswrf.sfc", inputs = ncep.inputs)
  solarR <- RNCEP::NCEP.aggregate(solarR, HOURS = FALSE, fxn = "mean")  # Average Flux for the whole day
  solarR <- RNCEP::NCEP.array2df(solarR, var.name = "solarR")
  solarR$solarR <- solarR$solarR * 24 * 60 * 60 * 1e-06  # To convert Wt/m2 to MJ/m2
  solarR <- aggregate(solarR ~ datetime, data = solarR, mean)  # mean of all nearby spatial locations
  solarR$datetime <- substr(solarR$datetime, 1, 10)
  
### T Maximum Data
  Tmax <- ncep.gather2(variable = "tmax.2m", inputs = ncep.inputs)
  Tmax <- RNCEP::NCEP.aggregate(Tmax, HOURS = FALSE, fxn = "max")
  Tmax <- RNCEP::NCEP.array2df(Tmax, var.name = "Tmax")
  Tmax <- aggregate(Tmax ~ datetime, data = Tmax, max)
  Tmax$datetime <- substr(Tmax$datetime, 1, 10)
  Tmax$Tmax <- Tmax$Tmax - 273
  
  
  ## T Minimum Data
  Tmin <- ncep.gather2(variable = "tmin.2m", inputs = ncep.inputs)
  Tmin <- RNCEP::NCEP.aggregate(Tmin, HOURS = FALSE, fxn = "max")
  Tmin <- RNCEP::NCEP.array2df(Tmin, var.name = "Tmin")
  Tmin <- aggregate(Tmin ~ datetime, data = Tmin, max)
  Tmin$datetime <- substr(Tmin$datetime, 1, 10)
  Tmin$Tmin <- Tmin$Tmin - 273
  
  ## Relative Humidity (I am using surface level, not Grid level to get rlative
  ## humidity, not absolute humidity, hope its not a problem.
  RH <- ncep.gather2(variable = c("rhum.sig995"), level = "surface", inputs = ncep.inputs)
  
  ## Warnign Message, not available in Reanalysis 2, Instead using Reanalysis 1.
  
  RHavg <- RNCEP::NCEP.aggregate(RH, HOURS = FALSE, fxn = "mean")
  RHmax <- RNCEP::NCEP.aggregate(RH, HOURS = FALSE, fxn = "max")
  RHmin <- RNCEP::NCEP.aggregate(RH, HOURS = FALSE, fxn = "min")
  RHavg <- RNCEP::NCEP.array2df(RHavg, var.name = "RH")
  RHmax <- RNCEP::NCEP.array2df(RHmax, var.name = "RH")
  RHmin <- RNCEP::NCEP.array2df(RHmin, var.name = "RH")
  
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
  Vwind <- RNCEP::NCEP.aggregate(Vwind, HOURS = FALSE, fxn = "mean")
  Vwind <- RNCEP::NCEP.array2df(Vwind, var.name = "Vwind")
  Vwind <- aggregate(Vwind ~ datetime, data = Vwind, mean)
  Vwind$datetime <- substr(Vwind$datetime, 1, 10)
  
  Uwind <- ncep.gather2(variable = "uwnd.10m", inputs = ncep.inputs)
  Uwind <- RNCEP::NCEP.aggregate(Uwind, HOURS = FALSE, fxn = "mean")
  Uwind <- RNCEP::NCEP.array2df(Uwind, var.name = "Uwind")
  Uwind <- aggregate(Uwind ~ datetime, data = Uwind, mean)
  Uwind$datetime <- substr(Uwind$datetime, 1, 10)
  
  Uwind$Uwind <- sqrt(Uwind$Uwind^2 + Vwind$Vwind^2)
  
  ## converting Windspeed from 10m to 2 m height using correlation
  ## provided by FAO (http://www.fao.org/docrep/X0490E/x0490e07.htm)
  Uwind$Uwind <- Uwind$Uwind * 4.87/log(67.8 * 10 - 5.42)  
  
  Uwind$Uwind <- Uwind$Uwind * (3600)/(1609)  # unit conversion from m/s to miles per hr
  
  
  ## Precipitation
  
  Rain <- ncep.gather2(variable = "prate.sfc", inputs = ncep.inputs)
  Rain <- RNCEP::NCEP.aggregate(Rain, HOURS = FALSE, fxn = "mean")
  Rain <- RNCEP::NCEP.array2df(Rain, var.name = "Rain")
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
##' @export
ncep.gather2 <- function (variable, level = "gaussian",
                          months.minmax = c(1, 12),
                          inputs,
                          reanalysis2 = TRUE,
                          return.units = FALSE) {
  years.minmax <- c(inputs$year1, inputs$year2)
  lat.southnorth <- c(inputs$lat, inputs$lat)
  lon.westeast <- c(inputs$lon, inputs$lon)
  
  if (is.null(level)) {
    stop("One of 'surface', 'gaussian', or a numeric pressure level must be given for 'level'")
  }
  if (length(level) > 1) {
    stop("Cannot access multiple reference systems in a single function call")
  }
  if (is.numeric(level) == FALSE) {
    if (level %in% c("surface", "gaussian") == FALSE) {
      stop("level must be one of 'gaussian', 'surface' or a numeric pressure level")
    }
  }
  if (reanalysis2 == TRUE && years.minmax[1] < 1979) {
    stop("The datetimes specified are out of range for the Reanalysis 2 dataset.")
  }
  if (years.minmax[1] < 1948) {
    stop("The datetimes specified are out of range.")
  }
  lon.westeast[1] <- ifelse(lon.westeast[1] < 0, 360 + lon.westeast[1], 
                            lon.westeast[1])
  lon.westeast[length(lon.westeast)] <- ifelse(lon.westeast[length(lon.westeast)] < 
                                               0, 360 + lon.westeast[length(lon.westeast)], lon.westeast[length(lon.westeast)])
  if (lon.westeast[1] > lon.westeast[length(lon.westeast)]) {
    cross.prime <- TRUE
  }  else {
    cross.prime <- FALSE
  }
  tlength <- NULL
  pb <- NULL
  
  if (cross.prime == FALSE) {
    if (is.numeric(level)) {
      out <- NCEP.gather.pressure(variable = variable, 
                                  months.minmax = months.minmax, years.minmax = years.minmax, 
                                  lat.minmax = lat.southnorth, lon.minmax = lon.westeast, 
                                  pressure = level, reanalysis2 = reanalysis2, 
                                  return.units = return.units, pb = pb, increments = tlength)
    }
    else if (level == "surface") {
      out <- NCEP.gather.surface(variable = variable, months.minmax = months.minmax, 
                                 years.minmax = years.minmax, lat.minmax = lat.southnorth, 
                                 lon.minmax = lon.westeast, reanalysis2 = reanalysis2, 
                                 return.units = return.units, pb = pb, increments = tlength)
    }
    else if (level == "gaussian") {
      out <- NCEP.gather.gaussian(variable = variable, 
                                  months.minmax = months.minmax, years.minmax = years.minmax, 
                                  lat.minmax = lat.southnorth, lon.minmax = lon.westeast, 
                                  reanalysis2 = reanalysis2, return.units = return.units, 
                                  pb = pb, increments = tlength)
    }
  }
  else if (cross.prime == TRUE) {
    if (is.numeric(level)) {
      out.west <- NCEP.gather.pressure(variable = variable, 
                                       months.minmax = months.minmax, years.minmax = years.minmax, 
                                       lat.minmax = lat.southnorth, lon.minmax = c(lon.westeast[1], 
                                                                      357.5), pressure = level, reanalysis2 = reanalysis2, 
                                       return.units = return.units, pb = pb, increments = tlength)
      out.east <- NCEP.gather.pressure(variable = variable, 
                                       months.minmax = months.minmax, years.minmax = years.minmax, 
                                       lat.minmax = lat.southnorth, lon.minmax = c(0, 
                                                                      lon.westeast[2]), pressure = level, reanalysis2 = reanalysis2, 
                                       return.units = return.units, pb = pb, increments = tlength)
      out <- NCEP.bind(data.west = out.west, data.east = out.east)
    }
    else if (level == "surface") {
      out.west <- NCEP.gather.surface(variable = variable, 
                                      months.minmax = months.minmax, years.minmax = years.minmax, 
                                      lat.minmax = lat.southnorth, lon.minmax = c(lon.westeast[1], 
                                                                     357.5), reanalysis2 = reanalysis2, return.units = return.units, 
                                      pb = pb, increments = tlength)
      out.east <- NCEP.gather.surface(variable = variable, 
                                      months.minmax = months.minmax, years.minmax = years.minmax, 
                                      lat.minmax = lat.southnorth, lon.minmax = c(0, 
                                                                     lon.westeast[2]), reanalysis2 = reanalysis2, 
                                      return.units = return.units, pb = pb, increments = tlength)
      out <- NCEP.bind(data.west = out.west, data.east = out.east)
    }
    else if (level == "gaussian") {
      out.west <- NCEP.gather.gaussian(variable = variable, 
                                       months.minmax = months.minmax, years.minmax = years.minmax, 
                                       lat.minmax = lat.southnorth, lon.minmax = c(lon.westeast[1], 
                                                                      358.125), reanalysis2 = reanalysis2, return.units = return.units, 
                                       pb = pb, increments = tlength)
      out.east <- NCEP.gather.gaussian(variable = variable, 
                                       months.minmax = months.minmax, years.minmax = years.minmax, 
                                       lat.minmax = lat.southnorth, lon.minmax = c(0, 
                                                                      lon.westeast[2]), reanalysis2 = reanalysis2, 
                                       return.units = return.units, pb = pb, increments = tlength)
      out <- NCEP.bind(data.west = out.west, data.east = out.east)
    }
  }
  return(out)
}
