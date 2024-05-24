#' qsub_parallel
#'
#' @param settings pecan settings object
#' @param files allow submit jobs based on job.sh file paths.
#' @param prefix used for detecting if jobs are completed or not.
#' @param sleep time (in second) that we wait each time for the jobs to be completed.
#' @param hybrid A Boolean argument decide the way of detecting job completion. If it's TRUE then we will detect both the outputted files and job ids on the server. If it's FALSE then we will only detect the job ids on the server.
#' @export
#' @examples
#' \dontrun{
#'   qsub_parallel(settings)
#' }
#' @author Dongchen Zhang
#' 
#' @importFrom foreach %dopar%
#' @importFrom dplyr %>%
qsub_parallel <- function(settings, files = NULL, prefix = "sipnet.out", sleep = 10, hybrid = TRUE) {
  if("try-error" %in% class(try(find.package("doSNOW"), silent = T))){
    PEcAn.logger::logger.info("Package doSNOW is not installed! Please install it and rerun the function!")
    return(0)
  }
  #declare variables within foreach section
  run <- NULL
  folder <- NULL
  run_list <- readLines(con = file.path(settings$rundir, "runs.txt"))
  is_local <- PEcAn.remote::is.localhost(settings$host)
  is_qsub <- !is.null(settings$host$qsub)
  is_rabbitmq <- !is.null(settings$host$rabbitmq)
  # loop through runs and either call start run, or launch job on remote machine
  # parallel submit jobs
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
    if (is_qsub) {
      jobids <- foreach::foreach(run = run_list, .packages="Kendall", .options.snow=opts, settings = rep(settings, length(run_list))) %dopar% {
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
    } else if (is_rabbitmq) {
      out <- foreach::foreach(run = run_list, .packages="Kendall", .options.snow=opts, settings = rep(settings, length(run_list))) %dopar% {
        run_id_string <- format(run, scientific = FALSE)
        PEcAn.remote::start_rabbitmq(file.path(settings$host$rundir, run_id_string), settings$host$rabbitmq$uri, settings$host$rabbitmq$queue)
      }
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
  folders <- file.path(settings$host$outdir, run_list)
  L_folder <- length(folders)
  pb <- utils::txtProgressBar(min = 0, max = L_folder, style = 3)
  #here we not only detect if the target files are generated.
  #we also detect if the jobs are still existed on the server.
  if (is_rabbitmq) {
    while ((L_folder - length(folders)) < L_folder) {
      Sys.sleep(sleep)
      completed_folders <- foreach::foreach(folder = folders) %dopar% {
        if(file.exists(file.path(folder, prefix))){
          return(folder)
        }
      } %>% unlist()
      folders <- folders[which(!folders %in% completed_folders)]
      pbi <- L_folder - length(folders)
      utils::setTxtProgressBar(pb, pbi)
    }
  } else {
    L_jobid <- length(jobids)
    pb1 <- utils::txtProgressBar(min = 0, max = L_jobid, style = 3)
    if (hybrid) {
      while ((L_folder - length(folders)) < L_folder & 
             (L_jobid - length(jobids)) < L_jobid) {
        Sys.sleep(sleep)
        completed_folders <- foreach::foreach(folder = folders) %dopar% {
          if(file.exists(file.path(folder, prefix))){
            return(folder)
          }
        } %>% unlist()
        folders <- folders[which(!folders %in% completed_folders)]
        
        #or we can try detect if the jobs are still on the server.
        #specify the host and qstat arguments for the future_map function.
        host <- settings$host
        qstat <- host$qstat
        completed_jobs <- jobids %>% furrr::future_map(function(id) {
          if (PEcAn.remote::qsub_run_finished(
            run = id,
            host = host,
            qstat = qstat)) {
            return(id)
          }
        }) %>% unlist()
        jobids <- jobids[which(!jobids %in% completed_jobs)]
        
        #compare two progresses and set the maximum progress for the progress bar.
        pbi <- L_folder - length(folders)
        utils::setTxtProgressBar(pb, pbi)
      }
    } else {
      #special case that only detect the job ids on the server.
      while ((L_jobid - length(jobids)) < L_jobid) {
        #detect if the jobs are still on the server.
        #specify the host and qstat arguments for the future_map function.
        Sys.sleep(sleep)
        host <- settings$host
        qstat <- host$qstat
        completed_jobs <- jobids %>% furrr::future_map(function(id) {
          if (PEcAn.remote::qsub_run_finished(
            run = id,
            host = host,
            qstat = qstat)) {
            return(id)
          }
        }) %>% unlist()
        jobids <- jobids[which(!jobids %in% completed_jobs)]
        
        #compare two progresses and set the maximum progress for the progress bar.
        pbi1 <- L_jobid - length(jobids)
        utils::setTxtProgressBar(pb1, pbi1)
      }
    }
  }
  close(pb)
  parallel::stopCluster(cl)
  PEcAn.logger::logger.info("Completed!")
}