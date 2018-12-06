#' Check if qsub run finished
#'
#' @param run run ID, as an integer
#' @param qstat (string) qstat command for checking job status
#' @inheritParams remote.execute.cmd
#'
#' @return `TRUE` if run is marked as DONE, otherwise FALSE.
#' @export
qsub_run_finished <- function(run, host, qstat) {
  if (is.na(run)) {
    PEcAn.logger::logger.warn("Job", run, "encountered an error during submission.",
                              "NOTE that the job will be stamped as 'finished' in BETY.")
    return(FALSE)
  }
  run_id_string <- format(run, scientific = FALSE)
  check <- gsub("@JOBID@", run, qstat)
  if (is.localhost(host)) {
    # Need to use `system` to allow commands with pipes
    out <- system(check, intern = TRUE, ignore.stdout = FALSE, ignore.stderr = FALSE, wait = TRUE)
  } else {
    # This uses `system2` under the hood, but that's OK because the entire 
    # command is passed as a single quoted argument, so the pipes are 
    # preserved.
    out <- remote.execute.cmd(host = host, cmd = check, stderr = TRUE)
  }

  if (length(out) > 0 && substring(out, nchar(out) - 3) == "DONE") {
    PEcAn.logger::logger.debug("Job", run, "for run", run_id_string, "finished")
    return(TRUE)
  } else {
    return(FALSE)
  }
}
