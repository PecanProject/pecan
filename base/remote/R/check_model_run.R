#' Check if model run was successful
#'
#' @param out Output from model execution, as a character.
#' @param stop.on.error Throw error if _any_ of the runs fails. Default TRUE.
#'
#' @return `TRUE` if model run succeeded. If model run failed, throw an error if `stop.on.error`, or return FALSE.
#' @export
check_model_run <- function(out, stop.on.error = TRUE) {
  if ("ERROR IN MODEL RUN" %in% out) {
    success <- FALSE
    msg <- paste0("Model run aborted with the following error:\n", out)
    if (stop.on.error) {
      PEcAn.logger::logger.severe(msg)
    } else {
      PEcAn.logger::logger.error(msg)
    }
  } else {
    success <- TRUE
  }
  return(success)
}
