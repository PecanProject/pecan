# Date DOY relative to the assigned phenology year.
# 1 = Jan 1 of that year. Negative = prior calendar year (MSLSP OGI often
# sits in Nov-Dec). Same convention as match_landiq_mslsp.R doy_to_date().
# Do not use lubridate::yday(): averaging yday across Jan 1 yields a fake June.

.doy_abs_max <- 730

phenology_doy_from_date <- function(x, year) {
  d <- as.Date(x)
  yr <- as.integer(year)
  n <- length(d)
  if (length(yr) == 1L) {
    yr <- rep(yr, n)
  } else if (length(yr) != n) {
    yr <- rep(yr, length.out = n)
  }
  origin <- as.Date(sprintf("%d-01-01", yr))
  out <- as.numeric(d - origin) + 1
  out[!is.finite(out) | abs(out) > .doy_abs_max] <- NA_real_
  out
}

phenology_doy_to_date <- function(year, doy) {
  yr <- as.integer(year)
  d <- suppressWarnings(as.numeric(doy))
  n <- max(length(yr), length(d))
  yr <- rep(yr, length.out = n)
  d <- rep(d, length.out = n)
  out <- as.Date(rep(NA_character_, n))
  ok <- !is.na(yr) & !is.na(d) & is.finite(d) & abs(d) <= .doy_abs_max
  if (any(ok)) {
    out[ok] <- as.Date(sprintf("%d-01-01", yr[ok])) +
      as.integer(round(d[ok])) - 1L
  }
  out
}

date_col_missing <- function(dt, col) {
  if (!col %in% names(dt)) {
    return(rep(TRUE, nrow(dt)))
  }
  is.na(as.Date(dt[[col]]))
}
