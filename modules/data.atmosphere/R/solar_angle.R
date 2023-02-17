#' Cosine of solar zenith angle
#'
#' For explanations of formulae, see https://web.archive.org/web/20180307133425/http://www.itacanet.org/the-sun-as-a-source-of-energy/part-3-calculating-solar-angles/
#'
#' @description
#' `cos_solar_zenith_angle` returns the cosine of solar zenith angle.
#' The cosine of the solar zenith angle (cossza) is the angle between the sun’s rays and the vertical.
#' This angle is important because it determines the intensity of solar radiation that reaches the Earth's surface.
#' When the solar zenith angle is small (i.e. the sun is close to directly overhead), the intensity of solar radiation is high.
#' When the solar zenith angle is larger (i.e. the sun is closer to the horizon), the intensity of solar radiation is lower, resulting in cooler temperatures.
#' It is required for photolysis.
#'
#' It depends upon the following -
#'
#' doy Day of year : It is the day when you are calculating the angle.
#' January 1st being day 1 and December 31st being day 365 (or 366 in leap years).
#' It is used to determine the position of the sun in the sky throughout the year.
#' Unit - Dimensionless (a simple count of the day within a year).
#'
#' hr Hours timestep : It is the time of day that you're trying to predict zenith angle for.
#' It's an integer value ranging from 0 to 24.
#' hour time step might be used to represent the change in solar position and angle between consecutive hours within a day.
#' Unit - Hours (h)
#'
#' dt Timestep : It represents the time interval between two values.
#' Therefore it is an integer value ranging from 0 to 60.
#' It should have a default value of 0.
#' It is designed to calculate the midpoint between two time steps if value is passed in vectors of days and hours.
#' dt tells the function how far apart values in hours are.
#' a day time step might be used to represent the change in solar position and angle between consecutive days.
#' Unit - Minutes (min).
#'
#' lat Latitude : Latitude measures the distance north or south of the equator.
#' Unit - Degrees (°) in decimal format (e.g., 37.7749° for San Francisco).
#'
#' lon Longitude : Longitude measures distance east or west of the prime meridian.
#' Unit - Degrees (°) in decimal format.
#' With a positive sign for locations east of the prime meridian and a negative sign for locations west of the prime meridian.
#' (e.g., -122.4194° for San Francisco)
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
