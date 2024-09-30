#' Convert CF-style date-time to POSIXct date-time
#'
#' @param value Numeric value of CF date-time 
#' @param unit CF style unit (e.g. "days since 2010-01-01")
#' @param tz Time zone of result (default = "UTC")
#' @return POSIXct datetime
#' 
#' @export
#' 
#' @author Alexey Shiklomanov
#' 
#' @examples
#' cf2datetime(5, "days since 1981-01-01")
#' cf2datetime(27, "minutes since 1963-01-03 12:00:00 -05:00")
#' # no leap year
#' cf2datetime(365, "days since 1999-01-01")
#' # leap year
#' cf2datetime(365, "days since 2000-01-01 12:00:00 -05:00")
cf2datetime <- function(value, unit, tz = "UTC") {
  origin <- "1970-01-01 00:00:00 UTC"
  ctint <- ud_convert(value, unit, paste("seconds since", origin))
  result <- as.POSIXct(ctint, origin = origin)
  attr(result, "tzone") <- tz
  result
}

#' Convert POSIXct date-time to CF-style date-time
#'
#' @param datetime POSIXct datetime, or object that can be to POSIXct
#'   via `as.POSIXct`
#' @param unit Target CF-style unit (e.g. "days since 2010-01-01")
#' @param ... Additional arguments to `as.POSIXct`. A common one is
#'   `tz` for time-zone (e.g. `tz = "UTC"`).
#' @return Numeric value of date-time in target CF unit
#' 
#' @export
#' 
#' @examples
#' datetime2cf("1990-10-05", "days since 1990-01-01", tz = "UTC")
datetime2cf <- function(datetime, unit, ...) {
  if (!inherits(datetime, "POSIXct")) {
    datetime <- as.POSIXct(datetime, ...)
  }
  origin <- "1970-01-01 00:00:00 UTC"
  raw_ct <- as.numeric(datetime, origin = origin)
  ud_convert(raw_ct, paste("seconds since", origin), unit)
}

#' Extract Julian day from CF or POSIXct date-times
#'
#' This gets around the fact that most functions for calculating
#' Julian Day do not support non-integer days.
#'
#' @inheritParams cf2datetime
#' @inheritParams datetime2cf
#' @return Numeric Julian date
#' 
#' @export datetime2doy
#' 
#' @author Alexey Shiklomanov
#' 
#' @examples
#' datetime2doy("2010-01-01") # 1
#' datetime2doy("2010-01-01 12:00:00") # 1.5
#' cf2doy(0, "days since 2007-01-01") 
#' cf2doy(5, "days since 2010-01-01") # 6
#' cf2doy(5, "days since 2010-01-01") # 6
datetime2doy <- function(datetime, tz = "UTC") {
  if (!inherits(datetime, "POSIXct")) {
    as.POSIXct(datetime, tz = tz)
  }
  year <- as.numeric(strftime(datetime, "%Y", tz = tz))
  basedate <- as.POSIXct(paste0(year, "-01-01"), tz = tz)
  # Day 1 is 0 days from the basedate, so correct for that here
  as.numeric(difftime(datetime, basedate, tz = tz, units = "days")) + 1
}

#' Convert from CF to DOY
#' @rdname datetime2doy
#' @export cf2doy
#' 
#' @author Alexey Shiklomanov
#' 
cf2doy <- function(value, unit, tz = "UTC") datetime2doy(cf2datetime(value, unit, tz), tz)
