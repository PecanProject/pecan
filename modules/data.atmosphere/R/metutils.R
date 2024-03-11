## qc functions restricting to 'valid range' given in .nc meta-data
qctemp <- function(x) ifelse(x > 400 | x < 100, mean(x[x < 400 & x > 100]), x)
qcsolar <- function(x) ifelse(x < 0, 0, ifelse(abs(x) > 1300, mean(x[x < 1300]), x))
qcwind <- function(x) ifelse(abs(x) > 102, mean(abs(x[x < 102])), x)
qcprecip <- function(x) ifelse(x > 0.005 | x < 0, mean(x[x < 0.005 & x > 0]), x)
qcrh <- function(x) {
  return(ifelse(x > 100 | x < 0, mean(x[x < 100 & x > 0]), x))  # using logical range (0-100) rather than 'valid range (-25-125)'
} # qcrh

qcshum <- function(x) {
  x <- ifelse(x > 100 | x < 0, mean(x[x < 0.6553 & x > 0]), x)
  return(x[is.na(x)] <- mean(x, na.rm = TRUE))
} # qcshum

##' Convert specific humidity to relative humidity
##'
##' converting specific humidity into relative humidity
##' NCEP surface flux data does not have RH
##' from Bolton 1980 Teh computation of Equivalent Potential Temperature
##' \url{https://archive.eol.ucar.edu/projects/ceop/dm/documents/refdata_report/eqns.html}
##' @title qair2rh
##' @param qair specific humidity, dimensionless (e.g. kg/kg) ratio of water mass / total air mass
##' @param temp degrees C
##' @param press pressure in mb
##' @return rh relative humidity, ratio of actual water mixing ratio to saturation mixing ratio
##' @export
##' @author David LeBauer
qair2rh <- function(qair, temp, press = 1013.25) {
  es <- 6.112 * exp((17.67 * temp) / (temp + 243.5))
  e <- qair * press / (0.378 * qair + 0.622)
  rh <- e / es
  rh[rh > 1] <- 1
  rh[rh < 0] <- 0
  return(rh)
} # qair2rh

##' converts relative humidity to specific humidity
##' @title RH to SH
##' @param rh relative humidity (proportion, not \%)
##' @param T absolute temperature (Kelvin)
##' @param press air pressure (Pascals)
##' @export
##' @author Mike Dietze, Ankur Desai
##' @aliases rh2rv
rh2qair <- function(rh, T, press = 101325) {
  stopifnot(T[!is.na(T)] >= 0)
  Tc <- PEcAn.utils::ud_convert(T, "K", "degC")
  es <- 6.112 * exp((17.67 * Tc) / (Tc + 243.5))
  e <- rh * es
  p_mb <- press / 100
  qair <- (0.622 * e) / (p_mb - (0.378 * e))
  ## qair <- rh * 2.541e6 * exp(-5415.0 / T) * 18/29
  return(qair)
} # rh2qair

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
get.vpd <- function(rh, temp) {
  ## calculate saturation vapor pressure
  es <- get.es(temp)
  ## calculate vapor pressure deficit
  return(((100 - rh)/100) * es)
} # get.vpd

##' Calculate saturation vapor pressure
##'
##' @title get es
##' @param temp temperature in degrees C
##' @return saturation vapor pressure in mb
##' @export
##' @author David LeBauer
##' @examples
##' temp <- -30:30
##' plot(temp, get.es(temp))
get.es <- function(temp) {
  return(6.11 * exp((2500000/461) * (1/273 - 1/(273 + temp))))
} # get.es

## TODO: merge SatVapPress with get.es; add option to choose method
SatVapPres <- function(T) {
  # /estimates saturation vapor pressure (kPa) Goff-Gratch 1946 /input: T = absolute temperature
  T_st <- 373.15  ##steam temperature (K)
  e_st <- 1013.25  ##/saturation vapor pressure at steam temp (hPa)
  return(0.1 * exp(-7.90298 * (T_st/T - 1) + 5.02808 * log(T_st/T) - 1.3816e-07 * (10^(11.344 * (1 - T/T_st)) -
    1) + 0.0081328 * (10^(-3.49149 * (T_st/T - 1)) - 1) + log(e_st)))
} # SatVapPres


##' Calculate RH from temperature and dewpoint
##'
##' Based on equation 12 in Lawrence 2005, The Relationship between
##' Relative Humidity and the Dewpoint Temperature in Moist Air
##' A Simple Conversion and Applications. BAMS
##' https://doi.org/10.1175/BAMS-86-2-225
##' R = 461.5 K-1 kg-1 gas constant H2O
##' L enthalpy of vaporization 
##' linear dependence on T (p 226, following eq 9)
##' 
##' @title get RH
##' @param T air temperature, Kelvin
##' @param Td dewpoint, Kelvin
##' @return Relative Humidity numeric vector
##' @export
##' @author David LeBauer
get.rh <- function(T, Td) {
  if(Td >= T){
    rh <- 100
  } else {
    Rw <- 461.5 # gas constant for water vapor, J K-1 kg-1
    L <- 2.501e6 + (T-273.15) * (-2430) 
    arg <- -L / (Rw * T * Td) * (T - Td)
    rh <- 100 * exp(arg)
  }
  return(rh)
} # get.rh

##' Convert raster to lat, lon, var
##' @title Wide to Long
##' @param data.wide data
##' @param lat latitude for rows
##' @param lon longitude for columns
##' @param var variable being measured
##' @return data.frame with colnames (lat, lon, var)
##' @export
##' @author David LeBauer
wide2long <- function(data.wide, lat, lon, var) {
  colnames(data.wide) <- lon
  data.wide <- cbind(lat, data.wide)
  data.long <- reshape2::melt(data.wide, id = "lat")
  colnames(data.long) <- c("lat", "lon", var)
  data.long$lon <- as.numeric(as.character(data.long$lon))
  return(data.long)
} # wide2long


##' convert PAR to PPFD
##'
##' Converts photosynthetically active radiation (PAR, units of Watts / m2) to
##' photosynthetic photon flux density (PPFD) in units of umol / m2 / s
##' From Campbell and Norman p151
##' PPFD = PAR * (J/m2/s) * (1 mol / 2.35e5 J)
##' 2.35e5 J / mol is the energy content of solar radiation in the PAR waveband
##' @title par2ppfd
##' @param watts PAR (W / m2)
##' @author David LeBauer
##' @export
##' @return PPFD (umol / m2 / s)
##' @author David LeBauer
par2ppfd <- function(watts) {
  ppfd <- watts/(2.35 * 10^5)
  return(PEcAn.utils::ud_convert(ppfd, "mol ", "umol"))
} # par2ppfd


##' Solar Radiation to PPFD
##'
##' Here the input is the total solar radiation
##' so to obtain in the PAR spectrum need to multiply by 0.486 From Campbell and Norman p151
##' This is based on the approximation that PAR is 0.45-0.50 of the total radiation
##'
##' @title SW to PAR
##' @author David LeBauer
##' @param sw shortwave radiation (W/m2 == J/m2/s)
##' @export
##' @return PAR W/m2
sw2par <- function(sw) {
  return(sw * 0.486)
} # sw2par

##' CF Shortwave to PPFD
##'
##' Cambell and Norman 1998 p 151, ch 10
##' @title SW to PPFD
##' @author David LeBauer
##' @export
##' @param sw CF surface_downwelling_shortwave_flux_in_air (W/m2)
##' @return PPFD umol /m2 / s
sw2ppfd <- function(sw) {
  par <- sw2par(sw)
  return(par2ppfd(par))
} # sw2ppfd


##' Solar Radiation to PPFD
##'
##' There is no easy straight way to convert MJ/m2 to mu mol photons / m2 / s (PAR).
##' Note: 1 Watt = 1J/s
##' The above conversion is based on the following reasoning
##' 0.12 is about how much of the total radiation is expected to ocurr during the hour of maximum insolation (it is a guesstimate)
##' 2.07 is a coefficient which converts from MJ to mol photons (it is approximate and it is taken from ...
##' Campbell and Norman (1998). Introduction to Environmental Biophysics. pg 151 'the energy content of solar radiation in the PAR waveband is 2.35 x 10^5 J/mol'
##' See also the chapter radiation basics (10)
##' Here the input is the total solar radiation so to obtain in the PAR spectrum need to multiply by 0.486
##' This last value 0.486 is based on the approximation that PAR is 0.45-0.50 of the total radiation
##' This means that 1e6 / (2.35e6) * 0.486 = 2.07
##' 1e6 converts from mol to mu mol
##' 1/3600 divides the values in hours to seconds
##'
##' @title MJ to PPFD
##' @author Fernando Miguez
##' @author David LeBauer
##' @param solarMJ MJ per day
##' @export
##' @return PPFD umol /m2 / s
solarMJ2ppfd <- function(solarMJ) {
  return((0.12 * solarMJ) * 2.07 * 1e+06 / 3600)
} # solarMJ2ppfd

##' estimated exner function
##' @title Exner function
##' @param pres  air pressure (Bar)
##' @export
##' @author Mike Dietze
exner <- function(pres) {
  return(1004 * pres ^ (287 / 1004))
} # exner

##' estimate air density from pressure, temperature, and humidity
##' @title Air Density
##' @param pres  air pressure (pascals)
##' @param T    air temperature (Kelvin)
##' @param rv   humidity
##' @export
##' @author Mike Dietze
AirDens <- function(pres, T, rv) {
  return(pres / (287 * T * (1 + 0.61 * rv)))
} # AirDens

##' calculate latent heat of vaporization for water
##'
##' @title Latent heat of vaporization
##' @param airtemp   air temperature (Kelvin)
##' @export
##' @author Istem Fer
##' @return lV   latent heat of vaporization (J kg-1)
get.lv <- function(airtemp = 268.6465) {
  airtemp_C <- PEcAn.utils::ud_convert(airtemp, "K", "degC")
  return((94.21 * (365 - airtemp_C) ^ 0.31249) * 4.183 * 1000)
} # get.lv
