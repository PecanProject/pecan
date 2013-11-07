library(BioCro, lib.loc = "/home/dlebauer/library/R")
library(data.table, lib.loc = "/home/dlebauer/library/R")
isleapyear<-function(year) (year%%400 == 0) | (year%%4==0 & !year%%100 == 0)
paste0 <- function(...) paste(..., sep = "")

args <- commandArgs(TRUE)
lati <- as.numeric(args[1])


load("/home/dlebauer/met/ncep/latlon.RData")


qair2rh <- function(qair, temp, press = 1013.25){
    es <-  6.112 * exp((17.67 * temp)/(temp + 243.5))
    e <- qair * press / (0.378 * qair + 0.622)
    rh <- e / es
    rh[rh > 1] <- 1
    rh[rh < 0] <- 0

    return(rh)
}


## qair2rh <- function(shum, temp){
##     ## converting specific humidity into relative humidity (surface flux data does not have RH
##     ## reference for calculation "The Rotronic Humidity HandBook"
##     mixingratio <- shum / (1 - shum) * 1000
##     ## using mixing ratio and atm pressure, find partial press of water in pascal
##     waterpartialpress <- (1e+5) * mixingratio / (621.9 + mixingratio) ## in pascal
##     ## saturated water vapor pressure using antoine equation
##     satwatervappress <- (8.07131 - (1730.63 / (244.485 + temp))) * 133 ## in pascal
##     rh <- (waterpartialpress) / (satwatervappress) ##RH fraction
##     rh[rh > 1] <- 1
##     rh[rh < 0] <- 0

##     return(rh / 100)
## }


for(loni in 1:192){
    
    Tmin = 0# Minimum temperature initialization

    currentlat <- round(Lat[lati], 2)
    currentlon <- round(Lon[loni], 2)
    weather.dir <- file.path("/home/dlebauer/met/ncep/",
                             paste0(abs(currentlat),
                                    ifelse(currentlat>0,"N", "S"), "x",
                                    abs(currentlon),
                                    ifelse(currentlon>0, "E", "W")))

    weather <- data.table(read.csv(file.path(weather.dir, "rawweather.csv")))

    library(udunits2)
    ## unit conversion
    Tmin <- min(weather$temp - 273.15, 0)

    ## qc functions restricting to "valid range" given in .nc meta-data
    qctemp   <- function(x) ifelse(x > 400 | x < 100, mean(x[x < 400 & x > 100]), x)
    qcsolar  <- function(x)ifelse(x<0, 0, ifelse(abs(x) > 1300, mean(x[x < 1300]), x))
    qcwind   <- function(x) ifelse(abs(x) > 102, mean(abs(x[x < 102])), x)
    qcprecip <- function(x) ifelse(x > 0.005 | x < 0 , mean(x[x < 0.005 & x >0], x))
    qcrh     <- function(x) ifelse(x > 100 | x < 0, mean(x[x < 100 & x>0]), x) ## using logical range (0-100) rather than "valid range (-25-125)"
    qcshum     <- function(x) ifelse(x > 100 | x < 0, mean(x[x < 0.6553 & x > 0]), x) 
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

    x <- weather[,list(year, day,
                       Tavg = qctemp(temp),
                       Tmax = qctemp(tempmax),
                       Tmin = qctemp(tempmin),
                       solarR   = ud.convert(qcsolar(solar),
                           "watt day",
                           "megajoule"), 
                       WS  = sqrt(qcwind(uwind)^2 + qcwind(vwind)^2),
                       precip = ud.convert(qcprecip(precip), "mm s-1", "mm day-1")),
                 qcshum(shum),
                 qcrh(rh)]


    x$RHmax <- x[, qair2rh(shum, Tmin)]
    x$RHmin <- x[, qair2rh(shum, Tmax)]
    x$RHavg <- x[, (RHmax + RHmin) / 2]
    


    forweach <- x[, list(year, day, solarR,
                         Tmax, Tmin, Tavg,
                         RHmax, RHmin, RHavg, WS,
                         precip)]
    ## fieldc=0.4
    ## wiltp=0.1
    ## mdep=2

    ##call weachNEW
    dat <- weachNEW(forweach,
                    lat = currentlat,
                    ts = 1,
                    temp.units = "Celsius",
                    rh.units = "fraction",
                    ws.units = "mps",
                    pp.units = "mm")
    write.csv(dat, file.path(weather.dir, "weather.csv"), row.names = FALSE)
}







