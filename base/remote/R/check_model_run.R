#' Check if model run was successful
#'
#' @param out Output from model execution, as a character.
#' @inheritParams start.model.runs
#'
#' @return
#' @export
check_model_run <- function(out, stop.on.error = TRUE) {
  if ("ERROR IN MODEL RUN" %in% out) {
    msg <- paste0("Model run aborted with the following error:\n", out)
    if (stop.on.error) {
      PEcAn.logger::logger.severe(msg)
    } else {
      PEcAn.logger::logger.error(msg)
    }
  }
}