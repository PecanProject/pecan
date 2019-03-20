#' Setup model launcher script and job list
#'
#' @inheritParams start_qsub
#' @param mpirun MPI info, usually from `settings$host$modellauncher$mpirun`
#' @param binary Binary info, usually from `settings$host$modellauncher$binary`
#'
#' @return NULL
#' @export
setup_modellauncher <- function(run, rundir, host_rundir, mpirun, binary) {
  run_string <- format(run, scientific = FALSE)
  run_id_dir <- file.path(rundir, run_string)
  launcherfile <- file.path(run_id_dir, "launcher.sh")
  if (file.exists(file.path(run_id_dir, "joblist.txt"))) {
    file.remove(file.path(run_id_dir, "joblist.txt"))
  }
  jobfile <- file(file.path(run_id_dir, "joblist.txt"), "w")

  writeLines(c("#!/bin/bash", paste(mpirun, binary, file.path(host_rundir, run_string, "joblist.txt"))),
             con = launcherfile)
  writeLines("./job.sh", con = jobfile)
  return(invisible(jobfile))
}
