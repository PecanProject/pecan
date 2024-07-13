#' Merge multiple job.sh files into one larger file.
#'
#' @param settings PEcAn.settings object with host section.
#' @param jobs_per_file the number of files you want to merge.
#' @param outdir output directory of merged job files.
#'
#' @return vector of the newly created filenames
#' @export
#' @author Dongchen Zhang
#'
merge_job_files <- function(settings, jobs_per_file = 10, outdir = NULL){
  # default outdir
  if(is.null(outdir)){
    outdir <- file.path(settings$rundir, "merged_jobs")
  }
  # create folder or delete previous job files.
  if(dir.exists(outdir)){
    unlink(list.files(outdir, recursive = T, full.names = T))
  }else{
    dir.create(outdir)
  }
  # merge job files.
  run_list <- readLines(con = file.path(settings$rundir, "runs.txt"))
  job_file_paths <- list.files(settings$host$rundir, pattern = "*.sh", recursive = T, full.names = T)
  i <- 0
  files <- c()
  while (i < length(job_file_paths)) {
    jobs <- c()
    for (j in 1:jobs_per_file) {
      if((i+j) > length(job_file_paths)){
        break
      }
      jobs <- c(jobs, readLines(job_file_paths[i+j]))
    }
    writeLines(jobs, con = file.path(outdir, paste0("job_", i,".sh")))
    Sys.chmod(file.path(outdir, paste0("job_", i,".sh")))
    files <- c(files, file.path(outdir, paste0("job_", i,".sh")))
    i <- i + jobs_per_file
  }
  files
}
