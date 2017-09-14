#' Stamp start and stop times of runs
#'
#' @param con BETY database connection
#' @param run (numeric) run ID
#'
#' @return `NULL`
#' @export
stamp_started <- function(con, run) {
  if (!is.null(con)) {
    run_id_string <- format(run, scientific = TRUE)
    db.query(query = paste("UPDATE runs SET started_at = NOW() WHERE id = ", run_id_string), con = con)
  } else {
    PEcAn.logger::logger.debug("Connection is null. Not actually writing timestamps to database")
  }
}

#' @rdname stamp_started
#' @export
stamp_finished <- function(con, run) {
  if (!is.null(con)) {
    run_id_string <- format(run, scientific = TRUE)
    db.query(query = paste("UPDATE runs SET finished_at = NOW() WHERE id = ", run_id_string), con = con)
  } else {
    PEcAn.logger::logger.debug("Connection is null. Not actually writing timestamps to database")
  }
}
