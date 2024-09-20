#' @description This function provides an option for submitting `write.configs` jobs to cluster.
#' @title parallel.write.configs
#' @param settings  PEcAn settings object.
#' @param ensemble.samples Lists: Sampled parameters for each plant functional types.
#' @param restart.list Lists: restart lists of each site from previous runs.
#' @author Dongchen Zhang.
parallel.write.configs <- function (settings, ensemble.samples, restart.list) {
  L <- length(settings)
  # grab info from settings.
  num.folder <- as.numeric(settings$state.data.assimilation$batch.settings$write.config$folder.num)
  cores <- as.numeric(settings$state.data.assimilation$batch.settings$write.config$cores)
  num.per.folder <- ceiling(L/num.folder)
  outdir <- settings$outdir
  # create folder for storing job outputs.
  batch.folder <- file.path(outdir, "batch")
  # delete the whole folder if it's not empty.
  if (file.exists(batch.folder)){
    unlink(batch.folder, recursive = T)
  } 
  dir.create(batch.folder)
  folder.paths <- job.ids <- c()
  for (i in 1:num.folder) {
    # create folder for each set of pixels.
    head.num <- (i-1)*num.per.folder + 1
    # if the site number can not be evenly divided.
    if (i*num.per.folder > L) {
      tail.num <- L
    } else {
      tail.num <- i*num.per.folder
    }
    folder.name <- paste0("From_", head.num, "_to_", tail.num)
    folder.path <- file.path(batch.folder, folder.name)
    folder.paths <- c(folder.paths, folder.path)
    if (dir.exists(folder.path)) {
      unlink(x = file.path(folder.path, c("stderr.log", "stdout.log")))
    } else {
      dir.create(folder.path)
      # write parameters.
      configs <- list(setting = settings[head.num:tail.num],
                      outdir = file.path(settings$outdir, "out"),
                      ensemble.samples = ensemble.samples,
                      restart.list = restart.list[head.num:tail.num])
      save(configs, file = file.path(folder.path, "configs.Rdata"))
    }
    jobsh <- c("#!/bin/bash -l", 
               "module load R/4.1.2", 
               "echo \"require (PEcAnAssimSequential)", 
               "      require (foreach)",
               "      write.configs.fun('@FOLDER_PATH@', '@CORES@', '@LIB@')", 
               "    \" | R --no-save")
    jobsh <- gsub("@FOLDER_PATH@", folder.path, jobsh)
    jobsh <- gsub("@CORES@", cores, jobsh)
    jobsh <- gsub("@LIB@", paste0("PEcAn.",settings$model$type), jobsh)
    writeLines(jobsh, con = file.path(folder.path, "job.sh"))
    # qsub command.
    qsub <- "qsub -l h_rt=48:00:00 -l buyin -pe omp @CORES@ -V -N @NAME@ -o @STDOUT@ -e @STDERR@ -S /bin/bash"
    qsub <- gsub("@CORES@", cores, qsub)
    qsub <- gsub("@NAME@", paste0("Job-", i), qsub)
    qsub <- gsub("@STDOUT@", file.path(folder.path, "stdout.log"), qsub)
    qsub <- gsub("@STDERR@", file.path(folder.path, "stderr.log"), qsub)
    qsub <- strsplit(qsub, " (?=([^\"']*\"[^\"']*\")*[^\"']*$)", perl = TRUE)
    cmd <- qsub[[1]]
    out <- system2(cmd, file.path(folder.path, "job.sh"), stdout = TRUE, stderr = TRUE)
    # grab job ids for future job completion detection.
    job.ids <- c(job.ids, PEcAn.remote::qsub_get_jobid(
      out = out[length(out)],
      qsub.jobid = settings$host$qsub.jobid,
      stop.on.error = TRUE))
  }
  # check outputs.
  PEcAn.logger::logger.info("Checking outputs.")
  l <- length(list.files(batch.folder, pattern = "out.configs.Rdata", recursive = T))
  pb <- utils::txtProgressBar(min = 0, max = num.folder, style = 3)
  while(l < num.folder) {
    Sys.sleep(60)
    # grab completed job ids from server.
    host <- settings$host
    qstat <- host$qstat
    completed_jobs <- job.ids %>% furrr::future_map(function(id) {
      if (PEcAn.remote::qsub_run_finished(
        run = id,
        host = host,
        qstat = qstat,
        verbose = FALSE)) {
        return(id)
      }
    }) %>% unlist()
    l <- length(list.files(batch.folder, pattern = "out.configs.Rdata", recursive = T))
    utils::setTxtProgressBar(pb, l)
    # if all jobs are finished.
    if (length(completed_jobs) == length(job.ids)) {
      break
    }
  }
  # if there is any job that is terminated by error.
  if (l < num.folder) {
    PEcAn.logger::logger.info(paste0("Something goes wrong within ", as.character(match.call()[[1]]), " job execution."))
    return(0)
  }
  Sys.sleep(10)
  # assemble results.
  PEcAn.logger::logger.info("Assembling results.")
  out.configs <- c()
  for (path in folder.paths) {
    res_env <- new.env()
    load(file.path(path, "out.configs.Rdata"), envir = res_env)
    out.configs <- c(out.configs, res_env$out.configs)
  }
  return(out.configs)
}

#' @description This function provides an option for splitting met files using cluster.
#' @title parallel.split.met
#' @param settings  PEcAn settings object.
#' @param my.split_inputs Character: Function to be called for splitting met.
#' @param conf.settings.before.split Lists: includes lists of configs for each site.
#' @param start.time Character: start time for cut met files.
#' @param stop.time Character: stop time for cut met files.
#' @param inputs List: physical paths to met files for each site.
#' @author Dongchen Zhang.
parallel.split.met <- function (settings, my.split_inputs, outdir, conf.settings.before.split, start.time, stop.time, inputs) {
  L <- length(settings)
  # grab info from settings.
  num.folder <- as.numeric(settings$state.data.assimilation$batch.settings$met.split$folder.num)
  cores <- as.numeric(settings$state.data.assimilation$batch.settings$met.split$cores)
  num.per.folder <- ceiling(L/num.folder)
  outdir <- settings$outdir
  # create folder for storing job outputs.
  batch.folder <- file.path(outdir, "batch")
  # delete the whole folder if it's not empty.
  if (file.exists(batch.folder)){
    unlink(batch.folder, recursive = T)
  } 
  dir.create(batch.folder)
  folder.paths <- job.ids <- c()
  for (i in 1:num.folder) {
    # create folder for each set of pixels.
    head.num <- (i-1)*num.per.folder + 1
    # if the site number can not be evenly divided.
    if (i*num.per.folder > L) {
      tail.num <- L
    } else {
      tail.num <- i*num.per.folder
    }
    folder.name <- paste0("From_", head.num, "_to_", tail.num)
    folder.path <- file.path(batch.folder, folder.name)
    folder.paths <- c(folder.paths, folder.path)
    if (dir.exists(folder.path)) {
      unlink(x = file.path(folder.path, c("stderr.log", "stdout.log")))
    } else {
      dir.create(folder.path)
      # write parameters.
      configs <- list(my.split_inputs = my.split_inputs,
                      outdir = file.path(outdir, "out"),
                      config.settings = conf.settings.before.split[head.num:tail.num],
                      start.time = start.time,
                      stop.time = stop.time,
                      inputs = inputs[head.num:tail.num],
                      nens = as.numeric(conf.settings.before.split[[1]]$ensemble$size))
      save(configs, file = file.path(folder.path, "configs.Rdata"))
    }
    jobsh <- c("#!/bin/bash -l", 
               "module load R/4.1.2", 
               "echo \"require (PEcAnAssimSequential)", 
               "      require (foreach)",
               "      met.split.fun('@FOLDER_PATH@', '@CORES@', '@LIB@')", 
               "    \" | R --no-save")
    jobsh <- gsub("@FOLDER_PATH@", folder.path, jobsh)
    jobsh <- gsub("@CORES@", cores, jobsh)
    jobsh <- gsub("@LIB@", paste0("PEcAn.",settings$model$type), jobsh)
    writeLines(jobsh, con = file.path(folder.path, "job.sh"))
    # qsub command.
    qsub <- "qsub -l h_rt=48:00:00 -l buyin -pe omp @CORES@ -V -N @NAME@ -o @STDOUT@ -e @STDERR@ -S /bin/bash"
    qsub <- gsub("@CORES@", cores, qsub)
    qsub <- gsub("@NAME@", paste0("Job-", i), qsub)
    qsub <- gsub("@STDOUT@", file.path(folder.path, "stdout.log"), qsub)
    qsub <- gsub("@STDERR@", file.path(folder.path, "stderr.log"), qsub)
    qsub <- strsplit(qsub, " (?=([^\"']*\"[^\"']*\")*[^\"']*$)", perl = TRUE)
    cmd <- qsub[[1]]
    out <- system2(cmd, file.path(folder.path, "job.sh"), stdout = TRUE, stderr = TRUE)
    # grab job ids for future job completion detection.
    job.ids <- c(job.ids, PEcAn.remote::qsub_get_jobid(
      out = out[length(out)],
      qsub.jobid = settings$host$qsub.jobid,
      stop.on.error = TRUE))
  }
  # check outputs.
  PEcAn.logger::logger.info("Checking outputs.")
  l <- length(list.files(batch.folder, pattern = "splits.Rdata", recursive = T))
  pb <- utils::txtProgressBar(min = 0, max = num.folder, style = 3)
  while(l < num.folder) {
    Sys.sleep(60)
    # grab completed job ids from server.
    host <- settings$host
    qstat <- host$qstat
    completed_jobs <- job.ids %>% furrr::future_map(function(id) {
      if (PEcAn.remote::qsub_run_finished(
        run = id,
        host = host,
        qstat = qstat,
        verbose = FALSE)) {
        return(id)
      }
    }) %>% unlist()
    l <- length(list.files(batch.folder, pattern = "splits.Rdata", recursive = T))
    utils::setTxtProgressBar(pb, l)
    # if all jobs are finished.
    if (length(completed_jobs) == length(job.ids)) {
      break
    }
  }
  # if there is any job that is terminated by error.
  if (l < num.folder) {
    PEcAn.logger::logger.info(paste0("Something goes wrong within ", as.character(match.call()[[1]]), " job execution."))
    return(0)
  }
  Sys.sleep(10)
  # assemble results.
  PEcAn.logger::logger.info("Assembling results.")
  inputs.split <- c()
  for (path in folder.paths) {
    res_env <- new.env()
    load(file.path(path, "splits.Rdata"), envir = res_env)
    inputs.split <- c(inputs.split, res_env$splits)
  }
  names(inputs.split) <- seq_along(conf.settings.before.split)
  return(inputs.split)
}

#' @description This function provides an option for reading sda outputs using cluster.
#' @title parallel.read.sda
#' @param settings  PEcAn settings object.
#' @param my.read_restart Character: Function to be called for reading sda outputs.
#' @param outdir Character: physical path to which model outputs are stored.
#' @param out.configs Lists: outputs returned by `parallel.write.configs` function.
#' @param stop.time Character: stop time for reading NC files.
#' @param var.names Character: names of variables to be read.
#' @param new.params Lists: outputs returned by `sda_matchparam` function.
#' @author Dongchen Zhang.
parallel.read.sda <- function(settings, my.read_restart, outdir, out.configs, stop.time, var.names, new.params) {
  L <- length(settings)
  # grab info from settings.
  num.folder <- as.numeric(settings$state.data.assimilation$batch.settings$sda.read$folder.num)
  cores <- as.numeric(settings$state.data.assimilation$batch.settings$sda.read$cores)
  num.per.folder <- ceiling(L/num.folder)
  outdir <- settings$outdir
  # create folder for storing job outputs.
  batch.folder <- file.path(outdir, "batch")
  # delete the whole folder if it's not empty.
  if (file.exists(batch.folder)){
    unlink(batch.folder, recursive = T)
  } 
  dir.create(batch.folder)
  folder.paths <- job.ids <- c()
  for (i in 1:num.folder) {
    # create folder for each set of pixels.
    head.num <- (i-1)*num.per.folder + 1
    # if the site number can not be evenly divided.
    if (i*num.per.folder > L) {
      tail.num <- L
    } else {
      tail.num <- i*num.per.folder
    }
    folder.name <- paste0("From_", head.num, "_to_", tail.num)
    folder.path <- file.path(batch.folder, folder.name)
    folder.paths <- c(folder.paths, folder.path)
    if (dir.exists(folder.path)) {
      unlink(x = file.path(folder.path, c("stderr.log", "stdout.log")))
    } else {
      dir.create(folder.path)
      # write parameters.
      configs <- list(my.read_restart = my.read_restart,
                      outdir = file.path(outdir, "out"),
                      out.configs = out.configs[head.num:tail.num],
                      stop.time = stop.time,
                      var.names = var.names,
                      new.params = new.params[head.num:tail.num],
                      nens = dim(out.configs[[1]]$runs)[1])
      save(configs, file = file.path(folder.path, "configs.Rdata"))
    }
    jobsh <- c("#!/bin/bash -l", 
               "module load R/4.1.2", 
               "echo \"require (PEcAnAssimSequential)", 
               "      require (foreach)",
               "      read.fun('@FOLDER_PATH@', '@CORES@', '@LIB@')", 
               "    \" | R --no-save")
    jobsh <- gsub("@FOLDER_PATH@", folder.path, jobsh)
    jobsh <- gsub("@CORES@", cores, jobsh)
    jobsh <- gsub("@LIB@", paste0("PEcAn.",settings$model$type), jobsh)
    writeLines(jobsh, con = file.path(folder.path, "job.sh"))
    # qsub command.
    qsub <- "qsub -l h_rt=48:00:00 -l buyin -pe omp @CORES@ -V -N @NAME@ -o @STDOUT@ -e @STDERR@ -S /bin/bash"
    qsub <- gsub("@CORES@", cores, qsub)
    qsub <- gsub("@NAME@", paste0("Job-", i), qsub)
    qsub <- gsub("@STDOUT@", file.path(folder.path, "stdout.log"), qsub)
    qsub <- gsub("@STDERR@", file.path(folder.path, "stderr.log"), qsub)
    qsub <- strsplit(qsub, " (?=([^\"']*\"[^\"']*\")*[^\"']*$)", perl = TRUE)
    cmd <- qsub[[1]]
    out <- system2(cmd, file.path(folder.path, "job.sh"), stdout = TRUE, stderr = TRUE)
    # grab job ids for future job completion detection.
    job.ids <- c(job.ids, PEcAn.remote::qsub_get_jobid(
      out = out[length(out)],
      qsub.jobid = settings$host$qsub.jobid,
      stop.on.error = TRUE))
  }
  # check outputs.
  PEcAn.logger::logger.info("Checking outputs.")
  l <- length(list.files(batch.folder, pattern = "reads.Rdata", recursive = T))
  pb <- utils::txtProgressBar(min = 0, max = num.folder, style = 3)
  while(l < num.folder) {
    Sys.sleep(60)
    # grab completed job ids from server.
    host <- settings$host
    qstat <- host$qstat
    completed_jobs <- job.ids %>% furrr::future_map(function(id) {
      if (PEcAn.remote::qsub_run_finished(
        run = id,
        host = host,
        qstat = qstat,
        verbose = FALSE)) {
        return(id)
      }
    }) %>% unlist()
    l <- length(list.files(batch.folder, pattern = "reads.Rdata", recursive = T))
    utils::setTxtProgressBar(pb, l)
    # if all jobs are finished.
    if (length(completed_jobs) == length(job.ids)) {
      break
    }
  }
  # if there is any job that is terminated by error.
  if (l < num.folder) {
    PEcAn.logger::logger.info(paste0("Something goes wrong within ", as.character(match.call()[[1]]), " job execution."))
    return(0)
  }
  Sys.sleep(10)
  # assemble results.
  PEcAn.logger::logger.info("Assembling results.")
  reads <- c()
  for (path in folder.paths) {
    res_env <- new.env()
    load(file.path(path, "reads.Rdata"), envir = res_env)
    reads <- c(reads, res_env$reads)
  }
  names(reads) <- seq_along(out.configs)
  return(reads)
}

#' @description This function provides an option for executing model or removing files using cluster.
#' @title job.sub
#' @param settings  PEcAn settings object.
#' @param outdir Character: physical path to which model outputs are stored.
#' @param rm.file Boolean: if we want to delete files.
#' the model execution will be submitted if it's `FALSE`.
#' @param only.nc Boolean: if we want to only delete NC files.
#' @author Dongchen Zhang.
job.sub <- function(settings, rm.file, only.nc) {
  L <- length(settings)
  # grab info from settings.
  num.folder <- as.numeric(settings$state.data.assimilation$batch.settings$general.job$folder.num)
  cores <- as.numeric(settings$state.data.assimilation$batch.settings$general.job$cores)
  num.per.folder <- ceiling(L/num.folder)
  outdir <- settings$outdir
  # create folder for storing job outputs.
  batch.folder <- file.path(outdir, "batch")
  # delete the whole folder if it's not empty.
  if (file.exists(batch.folder)){
    unlink(batch.folder, recursive = T)
  } 
  dir.create(batch.folder)
  job.ids <- c()
  for (i in 1:num.folder) {
    # create folder for each set of pixels.
    head.num <- (i-1)*num.per.folder + 1
    # if the site number can not be evenly divided.
    if (i*num.per.folder > L) {
      tail.num <- L
    } else {
      tail.num <- i*num.per.folder
    }
    folder.name <- paste0("From_", head.num, "_to_", tail.num)
    folder.path <- file.path(batch.folder, folder.name)
    if (dir.exists(folder.path)) {
      unlink(x = file.path(folder.path, c("stderr.log", "stdout.log")))
    } else {
      dir.create(folder.path)
      # write parameters.
      # iteration, sza, vza, psi, observed, wavelengths.
      folder.runs <- run.lists[head.num:tail.num,]
      if (rm.file) {
        folder.runs <- file.path(outdir, "out", folder.runs)
      } else {
        folder.runs <- file.path(outdir, "run", folder.runs)
      }
      save(folder.runs, file = file.path(folder.path, "runs.Rdata"))
    }
    # create job file.
    if (rm.file) {
      if (only.nc) {
        jobsh <- c("#!/bin/bash -l", 
                   "module load R/4.1.2", 
                   "echo \"require (PEcAnAssimSequential)", 
                   "      require (foreach)",
                   "      rm.files('@FOLDER_PATH@', '@CORES@')", 
                   "    \" | R --no-save")
      } else {
        jobsh <- c("#!/bin/bash -l", 
                   "module load R/4.1.2", 
                   "echo \"require (PEcAnAssimSequential)", 
                   "      require (foreach)",
                   "      rm.files('@FOLDER_PATH@', '@CORES@', 'FALSE')", 
                   "    \" | R --no-save")
      }
    } else {
      jobsh <- c("#!/bin/bash -l", 
                 "module load R/4.1.2", 
                 "echo \"require (PEcAnAssimSequential)", 
                 "      require (foreach)",
                 "      parallel.job.execution('@FOLDER_PATH@', '@CORES@')", 
                 "    \" | R --no-save")
    }
    jobsh <- gsub("@FOLDER_PATH@", folder.path, jobsh)
    jobsh <- gsub("@CORES@", cores, jobsh)
    writeLines(jobsh, con = file.path(folder.path, "job.sh"))
    # qsub command.
    qsub <- "qsub -l h_rt=48:00:00 -l buyin -pe omp @CORES@ -V -N @NAME@ -o @STDOUT@ -e @STDERR@ -S /bin/bash"
    qsub <- gsub("@CORES@", cores, qsub)
    qsub <- gsub("@NAME@", paste0("Job-", i), qsub)
    qsub <- gsub("@STDOUT@", file.path(folder.path, "stdout.log"), qsub)
    qsub <- gsub("@STDERR@", file.path(folder.path, "stderr.log"), qsub)
    qsub <- strsplit(qsub, " (?=([^\"']*\"[^\"']*\")*[^\"']*$)", perl = TRUE)
    cmd <- qsub[[1]]
    out <- system2(cmd, file.path(folder.path, "job.sh"), stdout = TRUE, stderr = TRUE)
    # grab job ids for future job completion detection.
    job.ids <- c(job.ids, PEcAn.remote::qsub_get_jobid(
      out = out[length(out)],
      qsub.jobid = settings$host$qsub.jobid,
      stop.on.error = TRUE))
  }
  # check outputs.
  PEcAn.logger::logger.info("Checking outputs.")
  l <- length(list.files(batch.folder, pattern = "result.txt", recursive = T))
  pb <- utils::txtProgressBar(min = 0, max = num.folder, style = 3)
  while(l < num.folder) {
    Sys.sleep(60)
    # grab completed job ids from server.
    host <- settings$host
    qstat <- host$qstat
    completed_jobs <- job.ids %>% furrr::future_map(function(id) {
      if (PEcAn.remote::qsub_run_finished(
        run = id,
        host = host,
        qstat = qstat,
        verbose = FALSE)) {
        return(id)
      }
    }) %>% unlist()
    l <- length(list.files(batch.folder, pattern = "result.txt", recursive = T))
    utils::setTxtProgressBar(pb, l)
    # if all jobs are finished.
    if (length(completed_jobs) == length(job.ids)) {
      break
    }
  }
  # if there is any job that is terminated by error.
  if (l < num.folder) {
    PEcAn.logger::logger.info(paste0("Something goes wrong within ", as.character(match.call()[[1]]), " job execution."))
    return(0)
  }
  Sys.sleep(10)
}

##' This function provides means to split large SDA analysis (MCMC) runs into separate `qsub` jobs.
##' Including job creation, submission, and assemble.
##' @title qsub_analysis_submission
##' @param settings  PEcAn settings object.
##' @param block.list list: MCMC configuration lists for the block SDA analysis.
##' 
qsub_analysis_submission <- function(settings, block.list) {
  L <- length(block.list)
  # grab info from settings.
  num.folder <- as.numeric(settings$state.data.assimilation$batch.settings$analysis$folder.num)
  cores <- as.numeric(settings$state.data.assimilation$batch.settings$analysis$cores)
  num.per.folder <- ceiling(L/num.folder)
  outdir <- settings$outdir
  # create folder for storing job outputs.
  batch.folder <- file.path(outdir, "batch")
  # delete the whole folder if it's not empty.
  if (file.exists(batch.folder)){
    unlink(batch.folder, recursive = T)
  } 
  dir.create(batch.folder)
  # loop over sub-folders.
  folder.paths <- job.ids <- c()
  PEcAn.logger::logger.info(paste("Submitting", num.folder, "jobs."))
  for (i in 1:num.folder) {
    # create folder for each set of job runs.
    # calculate start and end index for the current folder.
    head.num <- (i-1)*job.per.folder + 1
    if (i*job.per.folder > L) {
      tail.num <- L
    } else {
      tail.num <- i*job.per.folder
    }
    # naming and creating folder.
    folder.name <- paste0("From_", head.num, "_to_", tail.num)
    folder.path <- file.path(batch.folder, folder.name)
    folder.paths <- c(folder.paths, folder.path)
    dir.create(folder.path)
    # save corresponding block list to the folder.
    blocks <- block.list[head.num:tail.num]
    save(blocks, file = file.path(folder.path, "block.Rdata"))
    # create job file.
    jobsh <- c("#!/bin/bash -l", 
               "module load R/4.1.2", 
               "echo \"require (PEcAnAssimSequential)", 
               "      require (foreach)", 
               "      qsub_analysis('@FOLDER_PATH@', '@CORES@')", 
               "    \" | R --no-save")
    jobsh <- gsub("@FOLDER_PATH@", folder.path, jobsh)
    jobsh <- gsub("@CORES@", cores, jobsh)
    writeLines(jobsh, con = file.path(folder.path, "job.sh"))
    # qsub command.
    qsub <- "qsub -l h_rt=48:00:00 -l buyin -pe omp @CORES@ -V -N @NAME@ -o @STDOUT@ -e @STDERR@ -S /bin/bash"
    qsub <- gsub("@NAME@", paste0("Job-", i), qsub)
    qsub <- gsub("@STDOUT@", file.path(folder.path, "stdout.log"), qsub)
    qsub <- gsub("@STDERR@", file.path(folder.path, "stderr.log"), qsub)
    qsub <- gsub("@CORES@", cores, qsub)
    qsub <- strsplit(qsub, " (?=([^\"']*\"[^\"']*\")*[^\"']*$)", perl = TRUE)
    cmd <- qsub[[1]]
    out <- system2(cmd, file.path(folder.path, "job.sh"), stdout = TRUE, stderr = TRUE)
    # grab job ids for future job completion detection.
    job.ids <- c(job.ids, PEcAn.remote::qsub_get_jobid(
      out = out[length(out)],
      qsub.jobid = settings$host$qsub.jobid,
      stop.on.error = TRUE))
  }
  # checking results.
  PEcAn.logger::logger.info("Checking results.")
  # check outputs.
  l <- length(list.files(batch.folder, pattern = "result.txt", recursive = T))
  while(l < num.folder) {
    Sys.sleep(60)
    # grab completed job ids from server.
    host <- settings$host
    qstat <- host$qstat
    completed_jobs <- job.ids %>% furrr::future_map(function(id) {
      if (PEcAn.remote::qsub_run_finished(
        run = id,
        host = host,
        qstat = qstat,
        verbose = FALSE)) {
        return(id)
      }
    }) %>% unlist()
    l <- length(list.files(batch.folder, pattern = "result.txt", recursive = T))
    utils::setTxtProgressBar(pb, l)
    # if all jobs are finished.
    if (length(completed_jobs) == length(job.ids)) {
      break
    }
  }
  # if there is any job that is terminated by error.
  if (l < num.folder) {
    PEcAn.logger::logger.info(paste0("Something goes wrong within ", as.character(match.call()[[1]]), " job execution."))
    return(0)
  }
  # assemble results.
  PEcAn.logger::logger.info("Assembling results.")
  analysis <- c()
  for (path in folder.paths) {
    res_env <- new.env()
    load(file.path(path, "results.Rdata"), envir = res_env)
    analysis <- c(analysis, res_env$results)
  }
  names(analysis) <- names(block.list)
  return(analysis)
}