#' Get Job ID from qsub output
#'
#' @inheritParams check_model_run
#' @inheritParams start.model.runs
#' @param qsub.jobid (character) Regular expression string for extracting job ID from qsub output.
#' Usually from `settings$host$qsub.jobid`
#'
#' @return Job ID, as a string
#' @export
qsub_get_jobid <- function(out, qsub.jobid, stop.on.error) {
  qsub_worked <- grepl(qsub.jobid, out)
  if (!qsub_worked) {
    msg <- paste0("Job ID not assigned by qsub. The following qsub output may be relevant:\n", out)
    if (stop.on.error) {
      PEcAn.logger::logger.severe(msg)
    } else {
      PEcAn.logger::logger.error(msg)
    }
    jobid <- NA
  } else {
    jobid <- sub(qsub.jobid, '\\1', out)
  }
  return(jobid)
}

