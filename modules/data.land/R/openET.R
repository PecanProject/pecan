#' Extract daily ET data from OpenET
#'
#' Note that this requires the environment variable `OPENET_API_KEY` to be set.
#' A convenient way to do this is via a `.Renviron`, either globally
#' (`~/.Renviron`) or in the current working directory (`./.Renviron`), with
#' contents like:
#'
#' ```
#' OPENET_API_KEY="abcdefg123456"
#' ```
#'
#' You can obtain an OpenET API key from the OpenET data portal.
#'
#' @param design_points `data.frame` of design points with columns `lat` and `lon`
#' @param start_date Start date for data extraction
#' @param end_date End date for data extraction
#'
#' @return `design_points` `data.frame` with additional columns `date`, and
#' `et_mm_day` (ET, mm/day)
#' @export
extract_openet_daily <- function(design_points, start_date, end_date) {
  api_key <- Sys.getenv("OPENET_API_KEY")
  if (api_key == "") {
    stop("OPENET_API_KEY environment variable is not set")
  }

  start_date_str <- format(start_date, "%Y-%m-%d")
  end_date_str <- format(end_date, "%Y-%m-%d")

  request_body_template <- list(
    date_range = c(start_date_str, end_date_str),
    interval = "daily",
    model = "Ensemble",
    variable = "ET",
    reference_et = "gridMET",
    units = "mm",
    file_format = "JSON"
  )

  prep_request <- function(lon, lat) {
    request_body <- request_body_template
    request_body$geometry <- c(lon, lat)

    httr2::request("https://openet-api.org/raster/timeseries/point") |>
      httr2::req_headers(Authorization = api_key) |>
      httr2::req_body_json(request_body) |>
      httr2::req_throttle(capacity = 10, fill_time_s = 1) |>
      httr2::req_retry(max_tries = 3) |>
      httr2::req_timeout(seconds = 150)
  }

  raw_results <- design_points |>
    dplyr::mutate(
      reqs = purrr::map2(.data$lon, .data$lat, prep_request),
      resps = httr2::req_perform_parallel(
        raw_results[["reqs"]],
        max_active = 10,
        on_error = "continue"
      )
    )

  parse_response <- function(resp) {
    if (!inherits(resp, "httr2_response")) {
      return(NULL)
    }
    data <- httr2::resp_body_json(resp)
    if (length(data) == 0 || is.null(data[[1]]$time)) {
      return(NULL)
    }
    tibble::tibble(
      date = as.Date(purrr::map_chr(data, "time")),
      et_mm_day = purrr::map_dbl(data, "et")
    )
  }

  results <- raw_results |>
    dplyr::mutate(results = purrr::map(.data$resps, parse_response)) |>
    dplyr::select(-c("reqs", "resps")) |>
    tidyr::unnest("results", keep_empty = TRUE)

  results
}
