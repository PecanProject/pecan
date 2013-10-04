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







