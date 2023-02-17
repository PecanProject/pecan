#' Cosine of solar zenith angle
#'
#' For explanations of formulae, see https://web.archive.org/web/20180307133425/http://www.itacanet.org/the-sun-as-a-source-of-energy/part-3-calculating-solar-angles/
#'
#' @description
#' `cos_solar_zenith_angle` returns the cosine of solar zenith angle.
#' The cosine of the solar zenith angle (cossza) is the angle between the sun’s rays and the vertical
#' This solar zenith angle is normally used in combination with the solar azimuth angle to determine the position of the Sun as observed from a given location on the surface of the Earth.
#' The cosine of the solar zenith angle (cossza) is also a key component in Mean Radiant Temperature(MRT).
#' It is required for photolysis.
#'
#' doy Day of year : It is the day when you are calculating the angle.
#' lat Latitude : Latitude measures the distance north or south of the equator. The primary unit of latitude is degrees (°).
#' lon Longitude : Longitude measures distance east or west of the prime meridian. The primary unit of longitude is degrees (°).
#' dt Timestep : Timestep is a scalar quantity.
#' hr Hours timestep : It is a timestep in hours format.
#'
#' @references
#' https://www.ecmwf.int/sites/default/files/elibrary/2022/20336-calculating-cosine-solar-zenith-angle-thermal-comfort-indices.pdf
#' https://centaur.reading.ac.uk/104581/
#' https://wiki.seas.harvard.edu/geos-chem/index.php/Centralized_chemistry_time_step

#' @examplesIf interactive()
#' browseURL("https://www.ecmwf.int/sites/default/files/elibrary/2022/20336-calculating-cosine-solar-zenith-angle-thermal-comfort-indices.pdf")

#' @author Alexey Shiklomanov
#' @param doy Day of year
#' @param lat Latitude
#' @param lon Longitude
#' @param dt Timestep
#' @param hr Hours timestep
#' @return `numeric(1)` of cosine of solar zenith angle
#' @export
cos_solar_zenith_angle <- function(doy, lat, lon, dt, hr) {
  et <- equation_of_time(doy)
  merid <- floor(lon / 15) * 15
  merid[merid < 0] <- merid[merid < 0] + 15
  lc <- (lon - merid) * -4 / 60 ## longitude correction
  tz <- merid / 360 * 24 ## time zone
  midbin <- 0.5 * dt / 86400 * 24 ## shift calc to middle of bin
  t0 <- 12 + lc - et - tz - midbin ## solar time
  h <- pi / 12 * (hr - t0) ## solar hour
  dec <- -23.45 * pi / 180 * cos(2 * pi * (doy + 10) / 365) ## declination
  cosz <- sin(lat * pi / 180) * sin(dec) + cos(lat * pi / 180) * cos(dec) * cos(h)
  cosz[cosz < 0] <- 0
  return(cosz)
}

#' Equation of time: Eccentricity and obliquity
#'
#' For description of calculations, see https://en.wikipedia.org/wiki/Equation_of_time#Calculating_the_equation_of_time
#'
#' @author Alexey Shiklomanov
#' @param doy Day of year
#' @return `numeric(1)` length of the solar day, in hours.
#' @export
equation_of_time <- function(doy) {
  stopifnot(doy <= 367) # changed from 366 to 367 to account for leap years

  f <- pi / 180 * (279.5 + 0.9856 * doy)
  et <- (-104.7 * sin(f) + 596.2 * sin(2 * f) + 4.3 *
    sin(4 * f) - 429.3 * cos(f) - 2 *
    cos(2 * f) + 19.3 * cos(3 * f)) / 3600 # equation of time -> eccentricity and obliquity
  return(et)
}
