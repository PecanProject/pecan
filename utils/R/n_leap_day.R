##' @name n_leap_day
##' @title n_leap_day
##' @description number of leap days between two dates
##' @author Mike Dietze
##' @param start_date
##' @param end_date
##' @export
n_leap_day <- function(start_date, end_date) {
  library(lubridate)
  
  ## make sure dates are formatted correctly
  start_date <- as.Date(start_date)
  end_date   <- as.Date(end_date)
  
  ## which years are leap years?
  l_years <- leap_year(year(start_date):year(end_date))
  
  ## check for mid-year conditions in start/end
  if (start_date >= as.Date(paste0(year(start_date), "-03-01"))) {
    l_years[1] <- FALSE
  }
  if (end_date <= as.Date(paste0(year(end_date), "-02-28"))) {
    l_years[length(l_years)] <- FALSE
  }
  
  ## count up total number of leap days
  return(sum(l_years))
} # n_leap_day
