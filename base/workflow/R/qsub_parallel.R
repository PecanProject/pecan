#' qsub_parallel
#'
#' @param settings pecan settings object
#' @param files allow submit jobs based on job.sh file paths.
#' @param prefix used for detecting if jobs are completed or not.
#' @export
#' @examples
#' \dontrun{
#'   qsub_parallel(settings)
#' }
#' @author Dongchen Zhang
#' 
#' @importFrom foreach %dopar%
qsub_parallel <- function(settings, files = NULL, prefix = "sipnet.out") {
  #declare variables within foreach section
  run <- NULL
  folder <- NULL
  run_list <- readLines(con = file.path(settings$rundir, "runs.txt"))
  is_local <- PEcAn.remote::is.localhost(settings$host)
  # loop through runs and either call start run, or launch job on remote machine
  #parallel submit jobs
  cores <- parallel::detectCores()
  cl <- parallel::makeCluster(cores)
  doSNOW::registerDoSNOW(cl)
  #progress bar
  pb <- utils::txtProgressBar(min=1, max=ifelse(is.null(files), length(run_list), length(files)), style=3)
  progress <- function(n) utils::setTxtProgressBar(pb, n)
  opts <- list(progress=progress)
  PEcAn.logger::logger.info("Submitting jobs!")
  # if we want to submit jobs separately.
  if(is.null(files)){
    jobids <- foreach::foreach(run = run_list, .packages="Kendall", .options.snow=opts, settings = rep(settings, length(files))) %dopar% {
      run_id_string <- format(run, scientific = FALSE)
      qsub <- settings$host$qsub
      qsub <- gsub("@NAME@", paste0("PEcAn-", run_id_string), qsub)
      qsub <- gsub("@STDOUT@", file.path(settings$host$outdir, run_id_string, "stdout.log"), qsub)
      qsub <- gsub("@STDERR@", file.path(settings$host$outdir, run_id_string, "stderr.log"), qsub)
      qsub <- strsplit(qsub, " (?=([^\"']*\"[^\"']*\")*[^\"']*$)", perl = TRUE)
      # start the actual model run
      cmd <- qsub[[1]]
      if(PEcAn.remote::is.localhost(settings$host)){
        out <- system2(cmd, file.path(settings$host$rundir, run_id_string, "job.sh"), stdout = TRUE, stderr = TRUE)
      }else{
        out <- PEcAn.remote::remote.execute.cmd(settings$host, cmd, file.path(settings$host$rundir, run_id_string, "job.sh"), stderr = TRUE)
      }
      jobid <- PEcAn.remote::qsub_get_jobid(
        out = out[length(out)],
        qsub.jobid = settings$host$qsub.jobid,
        stop.on.error = TRUE)
      return(jobid)
    }
  }else{
    # if we want to submit merged job files.
    std_out <- file.path(settings$host$outdir, "merged_stdout")
    if(!dir.exists(std_out)){
      dir.create(std_out)
    }else{
      unlink(list.files(std_out, recursive = T, full.names = T))
    }
    jobids <- foreach::foreach(file = files, .packages="Kendall", .options.snow=opts, settings = rep(settings, length(files))) %dopar% {
      qsub <- settings$host$qsub
      base_name <- basename(file)
      num <- gsub("\\D", "", base_name)
      name <- paste0("SDA", num)
      qsub <- gsub("@NAME@", name, qsub)
      qsub <- gsub("@STDOUT@", file.path(std_out, paste0("stdout", num, ".log")), qsub)
      qsub <- gsub("@STDERR@", file.path(std_out, paste0("stderr", num, ".log")), qsub)
      qsub <- strsplit(qsub, " (?=([^\"']*\"[^\"']*\")*[^\"']*$)", perl = TRUE)
      cmd <- qsub[[1]]
      if(PEcAn.remote::is.localhost(settings$host)){
        out <- system2(cmd, file, stdout = TRUE, stderr = TRUE)
      }else{
        out <- PEcAn.remote::remote.execute.cmd(settings$host, cmd, file, stderr = TRUE)
      }
      jobid <- PEcAn.remote::qsub_get_jobid(
        out = out[length(out)],
        qsub.jobid = settings$host$qsub.jobid,
        stop.on.error = TRUE)
      return(jobid)
    }
  }
  PEcAn.logger::logger.info("Jobs submitted!")
  #check if jobs are completed
  PEcAn.logger::logger.info("Checking the qsub jobs status!")
  PEcAn.logger::logger.info(paste0("Checking the file ", prefix))
  ## setup progressbar
  pb <- utils::txtProgressBar(min = 0, max = length(unlist(run_list)), style = 3)
  pbi <- 0
  folders <- file.path(settings$host$outdir, run_list)
  while (length(folders) > 0) {
    Sys.sleep(10)
    completed_folders <- foreach::foreach(folder = folders, settings = rep(settings, length(run_list))) %dopar% {
      if(file.exists(file.path(folder, prefix))){
        return(folder)
      }
    }
    if(length(unlist(completed_folders)) > 0){
      Ind <- which(unlist(completed_folders) %in% folders)
      folders <- folders[-Ind]
      pbi <- pbi + length(completed_folders)
      utils::setTxtProgressBar(pb, pbi)
    }
  }
  close(pb)
  parallel::stopCluster(cl)
  PEcAn.logger::logger.info("Completed!")
}