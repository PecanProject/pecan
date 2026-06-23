#' Extract the first planting date of each crop cycle
#'
#' Reads a (JSON) management events file and finds the planting events at which
#' the site changes from from one crop to another, ignoring repeat plantings of
#' the same crop.
#' These are the dates when single-PFT models will need to restart to update
#' their crop parameterization.
#'
#' Requires each planting event to specify a `crop_code` attribute,
#' and reports a crop cycle change every time the crop code changes.
#' Note that crop codes are not required to match your model's PFT names,
#' so deciding how (or whether) to change parameterization on these dates
#' is up to the model operator.
#'
#' Also note that only _changes_ in crop code are detected:
#' If the event file contains no planting events the result has zero rows,
#' and any crop present before the first observed planting event
#' (say from initial conditions) is not reported.
#'
#' @param event_json path to an `events.json` file
#'
#' @return data frame with columns `site_id`, `date`, `crop`,
#'  with one row per detected crop cycle.
#' @author Chris Black
#'
#' @examples
#' evts <- system.file(
#'   "events_fixtures/events_site1_site2.json",
#'   package = "PEcAn.data.land"
#' )
#' events_to_crop_cycle_starts(evts)
#'
#' @export
events_to_crop_cycle_starts <- function(event_json) {
  jsonlite::read_json(event_json) |>
    purrr::map_dfr(get_plantings) |>
    dplyr::mutate(date = as.Date(.data$date)) |>
    find_crop_changes()
}



# Pull date and crop code out of planting events,
# ignoring all other event types
get_plantings <- function(site) {
  is_planting <- vapply(X = site$events,
                     FUN = \(x) x$event_type == "planting",
                     FUN.VALUE = logical(1))
  if(!any(is_planting)) {
    return(data.frame())
  }
  dates <- vapply(X = site$events[is_planting],
                  FUN = `[[`,
                  FUN.VALUE = character(1),
                  "date")
  codes <- vapply(X = site$events[is_planting],
                  FUN = `[[`,
                  FUN.VALUE = character(1),
                  "crop_code")

  data.frame(site_id = site$site_id, date = dates, crop_code = codes)
}




# helper for events_to_crop_cyle_starts,
# mostly to ease unit testing
find_crop_changes <- function(event_df) {
  if ("event_type" %in% colnames(event_df)) {
    event_df <- dplyr::filter(event_df, .data$event_type == "planting")
  }
  event_df |>
    dplyr::arrange(.data$site_id, .data$date) |>
    dplyr::mutate(crop_cycle_id = dplyr::consecutive_id(.data$site_id, .data$crop_code)) |>
    dplyr::slice_min(.data$date, by = c("site_id", "crop_cycle_id")) |>
    dplyr::select("site_id", "date", "crop_code")
}
