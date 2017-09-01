#' Equation of time: Eccentricity and obliquity
#'
#' @author Alexey Shiklomanov
#' @param doy Day of year
#' @export
eccentricity_obliquity <- function(doy) {
  stopifnot(doy <= 366)
  f      <- pi / 180 * (279.5 + 0.9856 * doy)
  et     <- (-104.7 * sin(f) + 596.2 * sin(2 * f) + 4.3 *
               sin(4 * f) - 429.3 * cos(f) - 2 *
               cos(2 * f) + 19.3 * cos(3 * f)) / 3600  # equation of time -> eccentricity and obliquity
  return(et)
}
