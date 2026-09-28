#' Compute first and last indices to extract from a Sipnet clim file
#'
#' Given vectors of integer years, integer days-of-year, and decimal hours,
#' computes the first and last indices that fall between the given start
#' and end times.
#'
#' Assumes the inputs are sorted (as Sipnet requires), and specifically that
#' the first and last rows are the earliest and latest timepoints.
#' If they are not, the result will be unpredictable.
#'
#' @param yrs,ydays,hrs vectors to be subset,
#'  containing respectively the year, day-of-year, and hour of day.
#' @param start_yr,start_yday,start_hr year, day-of-year, and hour
#'  to start from, all scalar
#' @param end_yr,end_yday,end_hr year, day-of-year, and hour
#'  to stop _before_, all scalar
#'
#' @author Chris Black
#'
sipnet_time_range_idx <- function(yrs, ydays, hrs,
                                start_yr, start_yday, start_hr,
                                end_yr, end_yday, end_hr) {

  # Motivation:
  # You may be reading this and asking "Does this task really need a 50-line
  #  function with 9 arguments? Why not convert the Sipnet times to datetimes
  #  and then simply do `dat[dt >= start & dt < end,]`?"
  # Because time handling is always slower and trickier than you think!
  #    * Converting everything to datetimes requires copying and parsing _all_
  #      rows of the input, while index matching can be done read-only.
  #    * All the known routes in R for for converting separate year, yday,
  #      and hour vectors to a single POSIXt pass through an explicit
  #      `paste(yr, yday, hr, min, sec)`, which requires a lot of temporary
  #      memory allocations and becomes slow when the data are large.
  #    * Additionally, conversion to and from POSIXt requires thinking about
  #      timezones, with all the bugs and headaches that come with them.
  #      This function avoids times and conversions entirely; start and stop
  #      values always mean exactly what they do in the input file.
  # This way benchmarks two orders of magnitude faster than the Sipnet time
  #  conversion approach it replaced. Real-world results may vary.

  nrow <- length(yrs)
  stopifnot(
    length(ydays) == nrow,
    length(hrs) == nrow,
    all(lengths(list(start_yr, start_yday, start_hr,
                     end_yr, end_yday, end_hr)) == 1)
  )

  # Cases where file begins on or after requested start time
  first_row <- 0
  if (start_yr < yrs[[1]]) {
    first_row <- 1
  } else if(start_yr == yrs[[1]]) {
    if (start_yday < ydays[[1]]) {
      first_row <- 1
    } else if (start_yday == ydays[[1]] && start_hr <= hrs[[1]]) {
      first_row <- 1
    }
  }
  if (first_row == 0) { # Start time is within file; find first matching row
    first_row <- match(
      TRUE,
      yrs > start_yr |
        (yrs == start_yr &
          (ydays > start_yday  | (ydays == start_yday & hrs >= start_hr)))
    )
    if (is.na(first_row)) {
      # no matches = file ended before start time => return 0
      return(list(start = 0, end = 0))
    }
  }

  # Cases where file ends before requested stop time
  last_row <- 0
  if (end_yr > yrs[[nrow]]) {
    last_row <- nrow
  } else if (end_yr == yrs[[nrow]]) {
    if (end_yday > ydays[[nrow]]) {
      last_row <- nrow
    } else if (end_yday == ydays[[nrow]] & end_hr > hrs[[nrow]]) {
      last_row <- nrow
    }
  }
  if (last_row == 0) { # End time is within file; find last matching row
    # Can't use match() here because that only gives the _first_ candidate
    last_row_candidates <- which(
      yrs < end_yr |
        (yrs == end_yr &
          (ydays < end_yday | (ydays == end_yday & hrs < end_hr)))
    )
    if (length(last_row_candidates) > 0) {
      last_row <- max(last_row_candidates)
    } else {
      # no candidates = file started after end => return 0
      return(list(start = 0, end = 0))
    }
  }

  list(start = first_row, end = last_row)
}
