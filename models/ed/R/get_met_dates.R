get_met_dates <- function(ed_metheader) {
  met_paths <- purrr::map(ed_metheader, "path_prefix")
  met_file_list <- purrr::map(met_paths, match_file)
  month_list <- purrr::map2(
    met_paths,
    met_file_list,
    ~gsub(normalizePath(.x, mustWork = FALSE), "", normalizePath(.y, mustWork = FALSE))
  )
  month_vec_raw <- tolower(gsub(".h5", "", Reduce(c, month_list)))
  month_vec <- lubridate::parse_date_time(month_vec_raw, "ym")
  date_list <- purrr::map(month_vec, dates_in_month)
  sort(Reduce(c, date_list))
}

dates_in_month <- function(date) {
  stopifnot(lubridate::mday(date) == 1)
  end_date <- date +
    lubridate::days(lubridate::days_in_month(date)) -
    lubridate::days(1)
  seq(date, end_date, by = "1 day")
}
