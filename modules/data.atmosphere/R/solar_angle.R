#' Cosine of Solar Zenith Angle
#'
#' Calculates the cosine of the solar zenith angle based on the given parameters. 
#' This angle is crucial in determining the amount of solar radiation reaching a point on Earth.
#'
#' For explanations of formulae, see https://web.archive.org/web/20180307133425/http://www.itacanet.org/the-sun-as-a-source-of-energy/part-3-calculating-solar-angles/
#'
#' @author Alexey Shiklomanov
#' @param doy Day of year. Integer representing the day of the year (1-365).
#' @param lat Latitude in degrees. Positive for the Northern Hemisphere and negative for the Southern Hemisphere.
#' @param lon Longitude in degrees. Positive for East and negative for West.
#' @param dt Time interval in seconds. Represents the duration over which the measurement is averaged or integrated.
#' @param hr Hour of the day (0-23). Specifies the specific hour for which the calculation is made.
#'
#' @return Numeric value representing the cosine of the solar zenith angle.
#'
#' @references 
#' "Understanding Solar Position and Solar Radiation" - RAMMB: [Link](https://rammb.cira.colostate.edu/wmovl/vrl/tutorials/euromet/courses/english/nwp/n5720/n5720005.htm)
#'
#' @examples
#' cos_solar_zenith_angle(doy = 150, lat = 45, lon = -93, dt = 3600, hr = 12)
#'
#' @export
cos_solar_zenith_angle <- function(doy, lat, lon, dt, hr) {
    et <- equation_of_time(doy)
    merid  <- floor(lon / 15) * 15
    merid[merid < 0] <- merid[merid < 0] + 15
    lc     <- (lon - merid) * -4/60  ## longitude correction
    tz     <- merid / 360 * 24  ## time zone
    midbin <- 0.5 * dt / 86400 * 24  ## shift calc to middle of bin
    t0   <- 12 + lc - et - tz - midbin  ## solar time
    h    <- pi/12 * (hr - t0)  ## solar hour
    dec  <- -23.45 * pi / 180 * cos(2 * pi * (doy + 10) / 365)  ## declination
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
  stopifnot(doy <= 367) #changed from 366 to 367 to account for leap years
  
  f      <- pi / 180 * (279.5 + 0.9856 * doy)
  et     <- (-104.7 * sin(f) + 596.2 * sin(2 * f) + 4.3 *
               sin(4 * f) - 429.3 * cos(f) - 2 *
               cos(2 * f) + 19.3 * cos(3 * f)) / 3600  # equation of time -> eccentricity and obliquity
  return(et)
}
