#' Start qsub runs
#'
#' @param run (numeric) run ID, as an integer
#' @param qsub_string qsub command string, with arguments. Usually from `settings$host$qsub`
#' @param rundir Local run directory. Usually from `settings$rundir`
#' @param host Remote host, as a list or character. Usually from `settings$host`.
#' @param host_rundir Remote host run directory. Usually from `settings$host$rundir`
#' @param host_outdir Remote host output directory. Usually from `settings$host$outdir`
#' @param stdout_log Logfile for redirecting `stdout`.
#' @param stderr_log Logfile for redirecting `stderr`
#' @param job_script Base name (no path) of script to run. Usually either `job.sh` or `launcher.sh`.
#' @param qsub_extra Extra `qsub` arguments. Usually from `settings$host$modellauncher$qsub.extra`
#'
#' @return Output of qsub command, as a character. This output can be parsed for ascertaining submission success.
#' @export
start_qsub <- function(run, qsub_string, rundir,
                       host, host_rundir, host_outdir,
                       stdout_log, stderr_log, job_script, qsub_extra = NULL) {
  
  run_id_string <- format(run, scientific = FALSE)
  
  if (!is.null(qsub_extra)) {
    qsub_string <- paste(qsub_string, qsub_extra)
  }
  qsub <- gsub("@NAME@", paste0("PEcAn-", run_id_string), qsub_string)
  qsub <- gsub("@STDOUT@", file.path(host_outdir, run_id_string, stdout_log), qsub)
  qsub <- gsub("@STDERR@", file.path(host_outdir, run_id_string, stderr_log), qsub)
  # NOTE: This converts `qsub` to a list.
  qsub <- strsplit(qsub, " (?=([^\"']*\"[^\"']*\")*[^\"']*$)", perl = TRUE)

  # start the actual model run
  cmd <- qsub[[1]]
  args <- qsub[-1]
  PEcAn.logger::logger.debug(cmd, args)
  if (is.localhost(host)) {
    out <- system2(cmd, c(args, file.path(rundir, run_id_string, job_script)), stdout = TRUE, stderr = TRUE)
    PEcAn.logger::logger.debug("Running locally:", run_id_string)
  } else {
    out <- remote.execute.cmd(host, cmd, c(args, file.path(host_rundir, run_id_string, job_script)), stderr = TRUE)
    PEcAn.logger::logger.debug("Running on remote", host, ":", run_id_string)
  }
  return(out)
}
