#' Extract sequence of dates from ED2IN file
#'
#' @inheritParams write_ed2in
#' @return Vector of dates from start date to end date by 1 day
#' @export
get_ed2in_dates <- function(ed2in) {
  ed2in_start_time <- ed2in2time(ed2in[["ITIMEA"]])
  start_date <- lubridate::make_datetime(
    ed2in[["IYEARA"]],
    ed2in[["IMONTHA"]],
    ed2in[["IDATEA"]],
    ed2in_start_time$hour,
    ed2in_start_time$minute
  )

  ed2in_end_time <- ed2in2time(ed2in[["ITIMEZ"]])
  end_date <- lubridate::make_datetime(
    ed2in[["IYEARZ"]],
    ed2in[["IMONTHZ"]],
    ed2in[["IDATEZ"]],
    ed2in_end_time$hour,
    ed2in_end_time$minute
  )
  lubridate::as_date(seq(start_date, end_date, by = "1 day"))
}

#' Convert ED2IN `ITIMEA/Z` string to hour and minute
#'
#' @param itimea ED2IN time string, e.g. "1200"
#' @return List containing numeric values of hour and minute
ed2in2time <- function(itimea) {
  timechar <- sprintf("%04d", itimea)
  list(
    hour = as.numeric(substr(timechar, 1, 2)),
    minute = as.numeric(substr(timechar, 3, 4))
  )
}
