#!/usr/bin/env Rscript

## Outputs the following data as .csv 
## shum:long_name = "mean Daily Specific Humidity at 2 m" ;
## shum:units = "kg/kg" ;
## rhum:long_name = "mean Daily relative humidity at sigma level 995" ;
## rhum:units = "%" ;
## prate:long_name = "mean Daily Precipitation Rate at surface" ;
## prate:units = "Kg/m^2/s" ;
## uwnd:long_name = "mean Daily u-wind at 10 m" ;
## uwnd:units = "m/s" ;
## vwnd:long_name = "mean Daily v-wind at 10 m" ;
## vwnd:units = "m/s" ;
## air:long_name = "mean Daily Air temperature at 2 m" ;
## air:units = "degK" ;
## dswrf:long_name = "mean Daily Downward Solar Radiation Flux at surface" ;
## dswrf:units = "W/m^2" ;


library(ncdf4)
library(lubridate)
library(data.table)
library(udunits2)
load("/home/dlebauer/dev/pecan/modules/data.atmosphere/data/ncep_landmask.RData")
source("/home/dlebauer/dev/rhwsd/inst/extdata/hwsd.r")

ncep.nc <-  nc_open("/home/dlebauer/met/ncep/allncep.nc")
time.idx <- ncvar_get(ncep.nc, "time")
date <- ymd("1948-01-01") + hours(time.idx - min(time.idx))
date <- date[date < ymd("2012-05-06")] ## valid range for temp; data goes above 200C after this date
time_steps <- length(date)
doy <- yday(date)
year <- year(date)
  
ncep_nc2dt <- function(lati, loni){

    currentlat <- round(Lat[lati], 2)
    currentlon <- round(Lon[loni], 2) - 180
    result <- list()
    
    vars <- list(year = year, doy = doy)
    for(var in names(ncep.nc$var)){
        vars[[var]] <- ncvar_get(nc = ncep.nc,
                                 varid = var,
                                 start = c(loni, lati, 1),
                                 count = c(1, 1, length(time.idx)))
    }
    
    result <- as.data.table(vars)
    return(result)   
}

qair2rh <- function(qair, temp, press = 1013.25){
    es <-  6.112 * exp((17.67 * temp)/(temp + 243.5))
    e <- qair * press / (0.378 * qair + 0.622)
    rh <- e / es
    rh[rh > 1] <- 1
    rh[rh < 0] <- 0

    return(rh)
}

## qc functions restricting to "valid range" given in .nc meta-data
qctemp   <- function(x) ifelse(x > 400 | x < 100, mean(x[x < 400 & x > 100]), x)
qcsolar  <- function(x) ifelse(x<0, 0, ifelse(abs(x) > 1300, mean(x[x < 1300]), x))
qcwind   <- function(x) ifelse(abs(x) > 102, mean(abs(x[x < 102])), x)
qcprecip <- function(x) ifelse(x > 0.005 | x < 0 , mean(x[x < 0.005 & x >0]), x)
qcrh     <- function(x) {
    ifelse(x > 100 | x < 0, mean(x[x < 100 & x>0]), x) ## using logical range (0-100) rather than "valid range (-25-125)"
}
qcshum     <- function(x){
    x <- ifelse(x > 100 | x < 0, mean(x[x < 0.6553 & x > 0]), x)
    x[is.na(x)] <- mean(x, na.rm = TRUE)
}
ncep_dt2weather <- function(weather = result){

    x <- weather[year < 2012 & year > 1948,
                 list(year, day = doy,
                      tavg = ud.convert(qctemp(air), "Kelvin", "Celsius"),
                      tmax = ud.convert(qctemp(tmax), "Kelvin", "Celsius"),
                      tmin = ud.convert(qctemp(tmin), "Kelvin", "Celsius"),
                      dswrf.MJ   = ud.convert(qcsolar(dswrf), "watt day", "megajoule"), 
                      wnd  = sqrt(qcwind(uwnd)^2 + qcwind(vwnd)^2),
                      precip.day = ud.convert(qcprecip(prate), "mm s-1", "mm day-1"),
                      shum = qcshum(shum),
                      rhum = qcrh(rhum))]
    
    
    x$rhmax <- x[, qair2rh(shum, tmin)]
    x$rhmin <- x[, qair2rh(shum, tmax)]
    x$rhavg <- x[, (rhmax + rhmin) / 2]

    forweach <- x[,list(year, day, dswrf.MJ,
                        tmax, tmin, tavg,
                        rhmax, rhmin, rhavg,
                        wnd, precip.day)]
 
    dat <- weachNEW(as.data.frame(forweach),
                    lat = currentlat,
                    ts = 1,
                    temp.units = "Celsius",
                    rh.units = "fraction",
                    ws.units = "mps",
                    pp.units = "mm")
    dat <- data.table(dat)
    return(dat)
}

for(lati in seq(Lat)){
    for(loni in seq(Lon)){
        here <- landmask[lat == Lat[lati] & lon == Lon[loni] - 180,]
        if(here$land){
            system.time(result <- ncep_nc2dt(lati, loni))
            system.time(weather <- ncep_dt2weather(result))
            ## get soil data
            ## here we run the model
        }
    }
}
     
