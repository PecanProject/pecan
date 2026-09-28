# nolint start
NO_EVENT_ENSEMBLE <- "_NO_EVENT_ENSEMBLE"
# nolint end

get_event_paths <- function(event_paths, parquet_dir = NULL) {
  if (missing(event_paths)) {
    if (is.null(parquet_dir)) {
      stop("Must provide either `event_paths` or `parquet_dir`")
    }
    event_paths <- list.files(
      parquet_dir,
      "\\.parquet$",
      full.names = TRUE,
      include.dirs = TRUE
    )
  }
  if (is.null(names(event_paths))) {
    names(event_paths) <- sub("\\.parquet$", "", basename(event_paths))
  }
  event_paths
}

#' Efficiently extract unique event IDs from parquet files
#'
#' @inheritParams event_parquet_to_json
#'
#' @return Named list of unique ensemble IDs for each event type. Names
#' correspond to event types.
#' @export
get_event_ensemble_ids <- function(
  event_paths,
  parquet_dir = NULL
) {
  event_paths <- get_event_paths(event_paths, parquet_dir)
  conn <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(conn))
  results <- list()
  for (event_type in names(event_paths)) {
    fname <- event_paths[[event_type]]
    ads <- arrow::open_dataset(fname)
    if (!("event_member_id" %in% names(ads))) {
      results[[event_type]] <- NO_EVENT_ENSEMBLE
      next
    }
    results[[event_type]] <- glue::glue(
      "SELECT DISTINCT event_member_id FROM read_parquet('{fname}')"
    ) |>
      dplyr::sql() |>
      dplyr::tbl(src = conn) |>
      dplyr::pull("event_member_id")
  }
  results
}

#' Convert PEcAn standard event parquet files to events.json
#'
#' Writes ensembles of PEcan events stored in parquet format to valid PEcAn
#' events.json files, based on a manifest table specification.
#'
#' Similar to the overall PEcAn ensemble structure, the
#' `events_ensemble_manifest` is a data frame that maps PEcAn ensemble IDs onto
#' ensembles of each individual event. The data frame must have the following
#' columns:
#'
#' - `ensemble_id` (character) --- PEcAn ensemble ID (used by PEcAn ensemble
#' code)
#' - `json_path` (character) --- Path to the events.json file that will be
#' written for this
#' ensemble.
#' - One column per event type (e.g., `irrigation`, `planting`, `harvest`,
#' etc.), with values (character) corresponding to values in the
#' `event_member_id` column in the corresponding files. The special value
#' `"_NO_EVENT_ENSEMBLE"` means that the parquet data for that event does not
#' have an `event_member_id` column (i.e., there is no ensemble analysis for
#' that event type).
#'
#' @param event_paths (character) Named character vector of paths to PEcAn
#' standard event parquet files. Names correspond to event types; e.g.,
#' `c(leafon = "/path/to/leaf_on.parquet")`. If not names are provided, they
#' are deduced from the file name (stripping the `.parquet` extension). Folders
#' containing parquet files are also supported.
#' @param events_ensemble_manifest `data.frame` that maps PEcAn ensemble IDs
#' and events.json paths onto ensembles of each event type. (See Details).
#' @param parquet_dir Alternative way to pass `event_paths` by just specifying
#' a directory of PEcAn standard event files. Each file or folder must be named
#' `<event_type>.parquet` (e.g., `irrigation.parquet/`, `leafon.parquet`).
#' @param site_ids (character) Optional vector of `site_id` values for
#' filtering.
#' @param start_date (Date or POSIXct) Optional start date for filtering
#' events
#' @param end_date (Date or POSIXct) Optional end date for filtering
#' events.
#' @param pecan_events_version (character) Version of the PEcAn events.json
#' standard to write to `events.json`. Default = 0.1.2.
#'
#' @return Nested tibble containing event data. `events.json` files are created
#' for each ensemble member as a side effect.
#' @export
event_parquet_to_json <- function(
  event_paths,
  events_ensemble_manifest,
  site_ids = NULL,
  start_date = NULL,
  end_date = NULL,
  parquet_dir = NULL,
  pecan_events_version = "0.1.2"
) {
  stopifnot(
    all(c("ensemble_id", "json_path") %in% colnames(events_ensemble_manifest))
  )
  event_types <- setdiff(
    colnames(events_ensemble_manifest),
    c("ensemble_id", "json_path")
  )
  all_events_list <- list()
  event_paths <- get_event_paths(event_paths, parquet_dir)
  conn <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(conn))
  for (event_type in event_types) {
    fname <- event_paths[[event_type]]
    dat <- dplyr::tbl(
      conn,
      dplyr::sql(glue::glue("SELECT * FROM read_parquet('{fname}')"))
    )
    if (!is.null(site_ids)) {
      dat <- dat |>
        dplyr::filter(.data$site_id %in% .env$site_ids)
    }
    if (!is.null(start_date)) {
      dat <- dat |>
        dplyr::filter(.data$date >= .env$start_date)
    }
    if (!is.null(end_date)) {
      dat <- dat |>
        dplyr::filter(.data$date <= .env$end_date)
    }
    pick_ensembles <- unique(events_ensemble_manifest[[event_type]])
    is_no_ens <- pick_ensembles == NO_EVENT_ENSEMBLE
    if (any(!is_no_ens)) {
      if (any(is_no_ens)) {
        stop(
          "In ", shQuote(event_type),
          ": Cannot mix ", NO_EVENT_ENSEMBLE,
          " with actual ensemble members."
        )
      }
      dat <- dat |>
        dplyr::filter(.data$event_member_id %in% .env$pick_ensembles)
    }
    dlocal <- dat |>
      dplyr::collect() |>
      dplyr::as_tibble()
    dnested <- dlocal |>
      dplyr::mutate(
        pecan_events_version = .env$pecan_events_version,
        event_type = .env$event_type,
        date = strftime(.data$date, "%Y-%m-%d"),
        site_id = as.character(.data$site_id)
      ) |>
      tidyr::nest(.key = "events", .by = dplyr::any_of(c(
        "pecan_events_version",
        "site_id",
        "event_member_id"
      ))) |>
      dplyr::mutate(
        events = purrr::map(.data$events, purrr::transpose)
      )
    if (!("event_member_id" %in% colnames(dnested))) {
      dnested <- dnested |>
        dplyr::mutate(event_member_id = NO_EVENT_ENSEMBLE)
    }
    manifest_join <- events_ensemble_manifest |>
      dplyr::select(dplyr::all_of(c(
        "ensemble_id",
        "json_path",
        event_member_id = event_type
      )))
    dfinal <- manifest_join |>
      dplyr::left_join(
        dnested,
        by = "event_member_id",
        relationship = "many-to-many"
      ) |>
      dplyr::select(-"event_member_id")
    all_events_list[[event_type]] <- dfinal
  }
  combined_events <- all_events_list |>
    dplyr::bind_rows() |>
    dplyr::summarize(
      events = list(purrr::list_c(.data$events)),
      .by = c("pecan_events_version", "site_id", "ensemble_id", "json_path")
    ) |>
    tidyr::nest(
      .key = "event_data",
      .by = c("ensemble_id", "json_path")
    )
  message("Writing event files")
  nevent <- nrow(combined_events)
  pb <- utils::txtProgressBar(0, nevent)
  for (irow in seq_len(nevent)) {
    drow <- combined_events[irow, ]
    event_data <- drow[["event_data"]][[1]]
    outfile <- drow[["json_path"]]
    jsonlite::write_json(event_data, outfile, pretty = TRUE, auto_unbox = TRUE)
    utils::setTxtProgressBar(pb, irow)
  }
  close(pb)
  combined_events
}
