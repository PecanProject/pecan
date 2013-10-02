##' Convert specific humidity to relative humidity
##'
##' converting specific humidity into relative humidity
##' NCEP surface flux data does not have RH
##' from Bolton 1980 Teh computation of Equivalent Potential Temperature 
##' \url{http://www.eol.ucar.edu/projects/ceop/dm/documents/refdata_report/eqns.html}
##' @title qair2rh
##' @param qair specific humidity, dimensionless (e.g. kg/kg) ratio of water mass / total air mass
##' @param temp degrees C
##' @param press pressure in mb
##' @return rh relative humidity, ratio of actual water mixing ratio to saturation mixing ratio
##' @export
##' @author David LeBauer
qair2rh <- function(qair, temp, press = 1013.25){
    es <-  6.112 * exp((17.67 * temp)/(temp + 243.5))
    e <- qair * press / (0.378 * qair + 0.622)
    rh <- e / es
    rh[rh > 1] <- 1
    rh[rh < 0] <- 0
    return(rh)
}
 
##' Calculate VPD
##'
##' Calculate vapor pressure deficit from relative humidity and temperature.
##' @title VPD
##' @param rh relative humidity, in percent 
##' @param temp temperature, degrees celsius
##' @return vpd: vapor pressure deficit, in mb
##' @export
##' @author David LeBauer
##' @examples
##' temp <- -30:30
##' plot(temp, get.vpd(0, temp))
get.vpd <- function(rh, temp){
  ## calculate saturation vapor pressure
  es <- get.es(temp)
  ## calculate vapor pressure deficit
  vpd <- ((100 - rh) / 100) * es
  return(vpd)
}
##' Calculate saturation vapor pressure
##'
##' @title get es
##' @param temp temperature in degrees C 
##' @return saturation vapor pressure in mb
##' @export
##' @author David LeBauer
##' @examples
##' temp <- -30:30
##' plot(temp, get.es(0, temp))
get.es <- function(temp){
  es <- 6.11 * exp((2.5e6 / 461) * (1 / 273 - 1 / (273 + temp)))
  return(es)
}
##' Calculate RH from temperature and dewpoint
##'
##' Based on equation 12 ( in Lawrence 2005, The Relationship between
##' Relative Humidity and the Dewpoint Temperature in Moist Air
##' A Simple Conversion and Applications.)
##' @title get RH
##' @param temp T in original equation
##' @param dewpoint Td in original 
##' @return numeric vector
##' @export
##' @author David LeBauer
get.rh <- function(T, Td){
  arg <- - L / (Rw * T * Td) * (T - Td)
  rh <- 100*exp(- L / (Rw * T * Td) * (T - Td))
}

##' Convert raster to lat, lon, var
##' @title Wide to Long
##' @param data.wide data
##' @param lat latitude for rows
##' @param lon longitude for columns
##' @param var variable being measured
##' @return data.frame with colnames (lat, lon, var)
##' @export
##' @author David LeBauer
wide2long <- function(data.wide, lat, lon, var){
  require(reshape)
  colnames(data.wide) <- lon
  data.wide <- cbind(lat, data.wide)
  data.long <- melt(data.wide, id = "lat")
  colnames(data.long) <- c("lat", "lon", var)
  data.long$lon <- as.numeric(as.character(data.long$lon))
  return(data.long)
}

