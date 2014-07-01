
cruncep_dt2weather <- function(weather = result, adjust=TRUE){

    x <- weather[,list(year, doy = doy, hour = hour,
                       solarR   = ppfd, 
                       DailyTemp.C = air_temperature,
                       RH = qair2rh(qair = specific_humidity,
                           temp = air_temperature, 
                           press = ud.convert(surface_pressure, "Pa", "mbar")),
                       WindSpeed  = wind,                       
                       precip = precipitation_flux)]
    return(x)
}

get.weather <- function(lat, lon, met.nc = met.nc, start.date, end.date){
#    if(!is.land(lat, lon)) stop("point is in ocean")
    result <- load.cfmet(lat = lat, lon = lon, met.nc = met.nc, start.date, end.date)
    hourly.result <- cruncep_hourly(result, lat = lat)
    weather <- cruncep_dt2weather(hourly.result)
}


##' Simple, Fast Daily to Hourly Climate Downscaling
##'
##' Based on weach family of functions but 5x faster than weachNEW,
##' and requiring metric units (temperature in celsius, windspeed in kph,
##' precip in mm, relative humidity as fraction)
##' @title weachDT
##' @param X data table with climate variables
##' @param lat latitude (for calculating solar radiation)
##' @param output.dt output timestep
##' @export
##' @return weather file for input to BioGro and related crop growth functions
##' @author David LeBauer
cfmet.downscale.daily <- weachDT <- function(dailymet, lat, output.dt = 1){
  
  tint <- 24 / output.dt
  tseq <- 0:(23 * output.dt) / output.dt
  ## Solar Radiation
  #   setnames(cfmet, 
  #            c("dswrf.MJ", "tmin", "tmax"),
  #            c("surface_downwelling_shortwave_flux_in_air",
  #              "air_temperature_min", "air_temperature_max"))
#   setnames(dailymet, 
#            c("air_temperature_min", "air_temperature_max"), 
#            c("tmin", "tmax"))
  setkeyv(dailymet, c("year", "doy"))
  
  
  light <- dailymet[,lightME(DOY = doy, t.d = tseq, lat = lat),
                    by = c("year", "doy")]
  
  light$Itot <- light[,list(I.dir + I.diff)]
  resC2 <- light[, list(resC2 = (Itot - min(Itot)) / max(Itot)), by = c("year", "doy")]$resC2
  solarR <- dailymet[,list(year, doy, 
                           solarR = rep(surface_downwelling_shortwave_flux_in_air * 2.07 * 10^5 /36000, each = tint) * resC2)]
                           
  SolarR <- cbind(resC2, solarR)[,list(SolarR = solarR * resC2)]$SolarR
  
  ## Temperature
  Temp <- dailymet[,list(Temp = tmin + (sin(2*pi*(tseq-10)/tint) + 1)/2 * (tmax - tmin), hour = tseq),
            by = 'year,doy']$Temp

  ## Relative Humidity
  RH <-   dailymet[,list(RH = rep(relative_humidity, each = tint), hour = tseq), by = 'year,doy']
  setkeyv(RH, c('year','doy','hour'))  
  qair <- dailymet[,list(year, doy, tmin, tmax, surface_pressure, air_temperature,
                         qmin = rh2qair(rh = relative_humidity/100, T = ud.convert(tmin, "celsius", "kelvin")),
                         qmax = rh2qair(rh = relative_humidity/100, T = ud.convert(tmax, "celsius", "kelvin")))]
  
  
  a <- qair[,list(year, doy, tmin, tmax, air_temperature, qmin, qmax, pressure = ud.convert(surface_pressure, "Pa", "millibar"))][
    ,list(year, doy, rhmin = qair2rh(qmin, air_temperature, pressure),
          rhmax = qair2rh(qmax, air_temperature, pressure))]
  rhscale <- (cos(2 * pi * (tseq - 10)/tint) + 1)/2
  RH <- a[, list(RH = rhmin + rhscale * (rhmax - rhmin)), by = c("year", 
                                                                 "doy")][, list(RH)]
  ## Wind Speed
  WS <- rep(dailymet$wind, each = tint)
  
  ## Precipitation
  precip <- rep(dailymet$precip/tint, each = tint)
  
  ## Hour
  time <- dailymet[,list(hour = tseq), by = c("year", "doy")]
  
  ans <- data.table(time, downwelling_photosynthetic_photon_flux = SolarR, air_temperature = Temp, 
                    relative_humidity = RH, wind = WS, precipitation_flux = precip)
  return(ans)
}

get.soil <- function(lat, lon, soil.nc = soil.nc){
    
    ## Lat and Lon
    Lat <- ncvar_get(soil.nc, "lat")
    Lon <- ncvar_get(soil.nc, "lon")

    lati <- which.min(abs(Lat - lat))
    loni <- which.min(abs(Lon - lon))

    ## topsoil
    usda_class <- ncvar_get(soil.nc, "t_usda_tex",
                            start = c(loni, lati),
                            count = c(1,1))
    ref_depth <- ud.convert(ncvar_get(soil.nc, "ref_depth",
                           start = c(loni, lati),
                           count = c(1, 1)), "cm", "m")
    return(list(usda_class = usda_class, ref_depth = ref_depth))
}

is.land <- function(lat, lon){
    Lat <- ncvar_get(nc = met.nc, varid = "lat")
    Lon <- ncvar_get(nc = met.nc, varid = "lon")
    lati <- which.min(abs(Lat-lat))
    loni <- which.min(abs(Lon-lon))
    mask <- ncvar_get(nc = met.nc, varid = "mask",
                      start = c(loni, lati), count = c(1,1))
    return(mask >= 0)
}

get.latlonbox <- function(lati, loni, Lat = Lat, Lon = Lon){
    lat <- c(mean(Lat[lati:(lati-1)]), mean(Lat[lati:(lati+1)]))
    lon <- c(mean(Lon[loni:(loni-1)]), mean(Lon[loni:(loni+1)]))
    return(c(sort(lat), sort(lon)))
}

get.cruncep <- function(lat, lon, start.date, end.date){
    result <- load.cfmet(lat, lon)
    hourly.result <- cruncep_hourly(result, lat = Lat[lati])
    weather <- cruncep_dt2weather(hourly.result)
    return(weather)
}


getNARRforBioCro<-function(lat,lon,year){
    USlayer<-read.table("/home/groups/ebimodeling/met/NARR/ProcessedNARR/NARRindex.txt")
    index <- which.min((lat - USlayer$Latt)^2 + (lon - USlayer$Lonn))
    i <- USlayer$Iindex[index]
    j <- USlayer$Jindex[index]
    filename <- paste("/home/groups/ebimodeling/met/NARR/ProcessedNARR/",year,formatC(i,width=3,flag=0),formatC(j,width=3,flag=0),".RData",sep="")
    load(filename)
    return(dat)
}
