#' Number of days in a year
#'
#' Calculate number of days in a year based on whether it is a leap year or not.
#'
#' @param year Numeric year (can be a vector)
#'
#' @author Alexey Shiklomanov
#' @return integer vector, all either 365 or 366
#' @export
#' @examples
#' days_in_year(2010)  # Not a leap year -- returns 365
#' days_in_year(2012)  # Leap year -- returns 366
#' days_in_year(2000:2008)  # Function is vectorized over years
days_in_year <- function(year) {
  if (any(year %% 1 != 0)) {
    PEcAn.logger::logger.severe("Year must be integer. Given ", year, '.')
  }
  ifelse(lubridate::leap_year(year), yes = 366, no = 365)
}
