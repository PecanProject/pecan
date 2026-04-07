#' Write a job.sh script for a PEcAn model run
#'
#' This helper function writes the job.sh shell script used to execute
#' a model run. It centralizes duplicated logic previously found in
#' individual write.config.* functions across model packages.
#'
#' @param rundir character. Path to the run directory
#' @param run.id character. The run ID
#' @param jobsh character vector. Lines of the job.sh script content
#' @param chmod logical. Whether to make job.sh executable. Default TRUE
#'
#' @return invisible path to the written job.sh file
#' @export
write_job_sh <- function(rundir, run.id, jobsh, chmod = TRUE) {
  job_path <- file.path(rundir, run.id, "job.sh")
  writeLines(jobsh, con = job_path)
  if (chmod) {
    Sys.chmod(job_path)
  }
  invisible(job_path)
}