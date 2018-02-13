#' Number of seconds in a given year
#'
#' @author Alexey Shiklomanov
#' @param year Numeric year (can be a vector)
#' @examples
#' seconds_in_year(2000)  # Leap year -- 366 x 24 x 60 x 60 = 31622400
#' seconds_in_year(2001)  # Regular year -- 365 x 24 x 60 x 60 = 31536000
#' seconds_in_year(2000:2005)  # Vectorized over year
#' @export
seconds_in_year <- function(year, leap_year = TRUE) {
    diy <- days_in_year(year, leap_year)
    siy <- udunits2::ud.convert(diy, 'days', 'seconds')
    return(siy)
}
