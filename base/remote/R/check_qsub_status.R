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
  check <- gsub("@JOBID", run, qstat)
  cmd_list <- strsplit(check, " (?=([^\"']*\"[^\"']*\")*[^\"']*$)", perl = TRUE)
  cmd <- cmd_list[[1]]
  args <- cmd_list[-1]
  if (is.localhost(host)) {
    out <- system2(cmd, args, stdout = TRUE, stderr = TRUE)
  } else {
    out <- remote.execute.cmd(host = host, cmd = cmd, args = args, stderr = TRUE)
  }

  if (length(out) > 0 && substring(out, nchar(out) - 3) == "DONE") {
    PEcAn.logger::logger.debug("Job", run, "for run", run_id_string, "finished")
    return(TRUE)
  } else {
    return(FALSE)
  }
}