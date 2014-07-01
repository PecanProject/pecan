##' Get time series vector from netCDF file
##'
##' internal convenience function for
##' streamlining extraction of data from netCDF files
##' with CF-compliant variable names
##' 
##' @title Get time series vector from netCDF file
##' @param var 
##' @param lati 
##' @param loni 
##' @param run.dates 
##' @param met.nc netcdf file with CF variable names
##' @return numeric vector
##' @author David Shaner LeBauer
get.ncvector <- function(var, lati = lati, loni = loni,
                      run.dates = run.dates, met.nc){
    
    start.idx = c(lat = lati, lon = loni, time = run.dates$index[1])
    count.idx = c(lat = 1, lon = 1, time = nrow(run.dates))
    dim.order <- sapply(met.nc$var$air_temperature$dim, function(x) x$name)
    ncvar_get2 <- function(var){
        ans <-  ncvar_get(nc = met.nc, varid = var,
                          start = start.idx[dim.order],
                          count = count.idx[dim.order])
        return(as.numeric(ans))
    }
    
    if(var %in% attributes(met.nc$var)$names){
        ans <- ncvar_get2(var)
    } else if (var == "surface_pressure"){
        ans <- 1013.25
    } else if (var == "wind"){
        ans <- sqrt(ncvar_get2("northward_wind")^2 + ncvar_get2("eastward_wind")^2)
    } else {
        ans <- NULL
    }
    return(ans)
}

cruncep_hourly <- function(result, lat){
    ## rename function to temporal_downscale?
    ## time step
    dt <- result[1:2,(as.duration(diff(date)))]
    dt_hr <- ud.convert(as.numeric(as.duration(dt)), "seconds", "hours")
    if(dt_hr >= 24){
        stop("only sub-daily downscaling supported")
    }

    new.date <- result[,list(hour = c(0:23)),
                       by = c("year", "month", "day", "doy")]

    new.date$date <- new.date[,list(date = ymd(paste(year, month, day)) + hours(hour))]
    
    ## tests
    ## min(result$date) == min(new.date$date)
    ## max(result$date) == max(new.date$date)

    ## converting surface_downwelling_shortwave_flux_in_air from W/m2 avg to PPFD

    solarMJ <- ud.convert(result$surface_downwelling_shortwave_flux_in_air, paste0("W ", dt_hr, "h"), "MJ")
    PAR <- 0.486 * solarMJ ## Cambell and Norman 1998 p 151, ch 10
    result$ppfd <- ud.convert(PAR, "mol s", "micromol h")

    hourly.result <- list()
    hourly.result[["surface_downwelling_shortwave_flux_in_air"]] <- result$surface_downwelling_shortwave_flux_in_air 
    hourly.result[["ppfd"]] <- result$ppfd
    for(var in c("surface_pressure", "specific_humidity",
                 "precipitation_flux", "air_temperature", "wind", "surface_downwelling_shortwave_flux_in_air", "ppfd")){
        if(var %in% colnames(result)){
            ## convert units from 6 hourly to hourly
            hrscale <- ifelse(var %in%
                              c("surface_downwelling_shortwave_flux_in_air",
                                "precipitation_flux"),
                              dt_hr, 1)
            
            f <- splinefun(as.numeric(result$date), (result[[var]] / hrscale), method = "monoH.FC")
            if(var == "air_temperature"){
                hourly.result[[var]] <- f(as.numeric(new.date$date))
            } else {
                hourly.result[[var]] <- f(as.numeric(new.date$date))
                hourly.result[[var]][hourly.result[[var]]<0] <- 0
            }
        }
    }


                                                            
    hourly.result <- cbind(new.date, as.data.table(hourly.result))#[date <= max(result$date),]
    
    if(hourly.result[,list(h = length(unique(hour))), by = c("year", "doy")][,all(unique(h) != 24)]){
        print(cruncep.file)
        print(hourly.result[,unique(year)])
        stop("some days don't have 24 hours")
    }
    return(hourly.result)
}

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
