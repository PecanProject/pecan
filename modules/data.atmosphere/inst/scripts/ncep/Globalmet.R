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


library(ncdf)

args <- commandArgs(TRUE)
lati <- as.numeric(args[1])
print(lati)


start.year <- 1948
end.year <- 2012

years <- seq(start.year, end.year)
dimyr <- length(years)

isleapyear<-function(year) (year%%400 == 0) | (year%%4==0 & !year%%100 == 0)
paste0 <- function(...) paste(..., sep = "")
##declare arrays for each of the five variables needed (ignore 366 days of leap year)

## To get lat:
## tmp0 <- open.ncdf(paste("/home/djaiswal/database/NCEP/soilmoisture-0-10cm/soilw.0-10cm.gauss.1948.nc",sep=""))
##    Lat<-get.var.ncdf(tmp0,"lat")
##    Lon<-get.var.ncdf(tmp0,"lon")
## save(Lat, Lon, file = "/home/dlebauer/met/ncep/latlon.RData")
load("/home/dlebauer/met/ncep/latlon.RData")


## soilm.0.10 <- newarray[,,,1]
## soilm.10.200 <- newarray[,,,1]
## tmp0 <- open.ncdf(paste("/home/djaiswal/database/NCEP/soilmoisture-0-10cm/soilw.0-10cm.gauss.",years[1],".nc",sep=""))
## soilm.0.10 <- get.var.ncdf(tmp0)
## close.ncdf(tmp0)
## tmp0<-open.ncdf(paste("/home/djaiswal/database/NCEP/soilmoisture-10-200cm/soilw.10-200cm.gauss.",years[1],".nc",sep=""))
## soilm.10.200 <- get.var.ncdf(tmp0)
## close.ncdf(tmp0)
## save(soilm.0.10, soilm.10.200, file = paste0("/home/dlebauer/met/ncep/soil.init.RData"))
## rm(soilm.0.10, soilm.10.200)

for(loni in 1:192){
    result <- list()

    currentlat <- round(Lat[lati], 2)
    currentlon <- round(Lon[loni], 2)
    print(currentlat)
    print(currentlon)
    for (i in seq(years)){
        year <- years[i]
        ndays <- ifelse(isleapyear(year), 366, 365)
        days <- 1:ndays
        
        shum.nc <- open.ncdf(paste("/home/djaiswal/database/NCEP/SpecificHumidity/shum.2m.gauss.",year,".nc",sep=""))
        shum <- get.var.ncdf(shum.nc, start = c(loni, lati, 1), count = c(1, 1, ndays))
        close.ncdf(shum.nc)

        rh.nc <- open.ncdf(paste("/home/djaiswal/database/NCEP/RelativeHumidity/rhum.sig995.",year,".nc",sep=""))
        rh <- get.var.ncdf(rh.nc, start = c(loni, lati, 1), count = c(1, 1, ndays))
        close.ncdf(rh.nc)

        
        tair.nc <- open.ncdf(paste("/home/djaiswal/database/NCEP/Temperature/air.2m.gauss.",year,".nc",sep=""))
        temp <- get.var.ncdf(tair.nc, start = c(loni, lati, 1), count = c(1, 1, ndays))
        close.ncdf(tair.nc)
        
        tmin.nc <- open.ncdf(paste("/home/djaiswal/database/NCEP/MinTemperature/tmin.2m.gauss.",year,".nc",sep=""))
        tempmin <- get.var.ncdf(tmin.nc, start = c(loni, lati, 1), count = c(1, 1, ndays))
        close.ncdf(tmin.nc)
        
        tmax.nc <- open.ncdf(paste("/home/djaiswal/database/NCEP/MaxTemperature/tmax.2m.gauss.",year,".nc",sep=""))
        tempmax <- get.var.ncdf(tmax.nc, start = c(loni, lati, 1), count = c(1, 1, ndays))
        close.ncdf(tmax.nc)
        
        uwind.nc <- open.ncdf(paste("/home/djaiswal/database/NCEP/WindspeedU/uwnd.10m.gauss.",year,".nc",sep=""))
        vwind.nc <- open.ncdf(paste("/home/djaiswal/database/NCEP/WindspeedV/vwnd.10m.gauss.",year,".nc",sep=""))
                                        #   need to combine these / calculate hyp. 
        vwind <- get.var.ncdf(uwind.nc, start = c(loni, lati, 1), count = c(1, 1, ndays))
        uwind <- get.var.ncdf(vwind.nc, start = c(loni, lati, 1), count = c(1, 1, ndays))
        close.ncdf(vwind.nc)
        close.ncdf(uwind.nc)

        
        solar.nc <- open.ncdf(paste("/home/djaiswal/database/NCEP/SolarRadiation/dswrf.sfc.gauss.",year,".nc",sep=""))
        solar <- get.var.ncdf(solar.nc, start = c(loni, lati, 1), count = c(1, 1, ndays))
        close.ncdf(solar.nc)
        
        prate.nc <- open.ncdf(paste("/home/djaiswal/database/NCEP/Precipitation/prate.sfc.gauss.",year,".nc",sep=""))
        precip <- get.var.ncdf(prate.nc, start = c(loni, lati, 1), count = c(1, 1, ndays))
        close.ncdf(prate.nc)


        result[[as.character(year)]] <- data.frame(year = rep(year,ndays), day = 1:ndays, shum, rh, temp, tempmin, tempmax, uwind, vwind, solar, precip) 
    }
    weather.dir <- file.path("/home/dlebauer/met/ncep/",
                             paste0(abs(currentlat),
                                    ifelse(currentlat>0,"N", "S"), "x",
                                    abs(currentlon),
                                    ifelse(currentlon>0, "E", "W")))
    dir.create(weather.dir, recursive = TRUE, showWarnings = FALSE)
    save(result,  file =  file.path(weather.dir, "rawweather.RData"))
    newresult <- do.call(rbind, result)
    write.csv(newresult, file =  file.path(weather.dir, "rawweather.csv"), row.names = FALSE)

