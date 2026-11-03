#' Create SIPNET event files from water balance data
#'
#' Aggregates irrigation to weekly values and formats for SIPNET.
#' Irrigation is summed by week and reported on the first day of each week.
#' Units are converted from mm to cm.
#'
#' @param df Data frame with columns: date, year, week, day_of_year, irr
#' @return Data frame with columns: loc, year, doy, event_type, irr_cm, type
#' @export
create_event_file <- function(df) {
  df |>
    dplyr::summarize(
      loc = 0,
      year = dplyr::first(.data$year),
      doy = dplyr::first(.data$day_of_year),
      event_type = "irrig",
      irr_mm_week = sum(.data$irr, na.rm = TRUE),
      type = 1,
      .by = c(.data$year, .data$week)
    ) |>
    dplyr::mutate(irr_cm = .data$irr_mm_week / 10) |>
    dplyr::select("loc", "year", "doy", "event_type", "irr_cm", "type")
}
