#' Start model execution in serial mode
#'
#' @inheritParams start_qsub
#'
#' @return Output of execution command, as a character (see [remote.execute.cmd()]).
#' @export
start_serial <- function(run, host, rundir, host_rundir, job_script) {
  run_id_string <- format(run, scientific = FALSE)
  if (is.localhost(host)) {
    out <- system2(file.path(rundir, run_id_string, job_script), stdout = TRUE, stderr = TRUE)
  } else {
    out <- remote.execute.cmd(host, file.path(host_rundir, run_id_string, job_script), stderr = TRUE)
  }
  return(out)
}
