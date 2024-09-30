#' n_leap_day
#'
#' number of leap days between two dates
#' @author Mike Dietze
#' @param start_date,end_date dates in any format recognized by \code{\link[base]{as.Date}}
#' @export
n_leap_day <- function(start_date, end_date) {
  
  ## make sure dates are formatted correctly
  start_date <- as.Date(start_date)
  end_date   <- as.Date(end_date)
  
  ## which years are leap years?
  l_years <- lubridate::leap_year(lubridate::year(start_date):lubridate::year(end_date))
  
  ## check for mid-year conditions in start/end
  if (start_date >= as.Date(paste0(lubridate::year(start_date), "-03-01"))) {
    l_years[1] <- FALSE
  }
  if (end_date <= as.Date(paste0(lubridate::year(end_date), "-02-28"))) {
    l_years[length(l_years)] <- FALSE
  }
  
  ## count up total number of leap days
  return(sum(l_years))
} # n_leap_day
