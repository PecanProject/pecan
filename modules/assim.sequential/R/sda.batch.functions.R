#' @description This function realizes the parallel `write.configs`.
#' @title write.configs.fun
#' @param folder.path Character: physical path to which the job file is located.
#' @param cores numeric: number of requested CPUs.
#' @param lib Character: extra required library.
#' @author Dongchen Zhang.
#' @importFrom foreach %dopar%
write.configs.fun <- function(folder.path, cores, lib) {
  configs <- readRDS(file.path(folder.path, "configs.rds"))
  # foreach.
  cl <- parallel::makeCluster(as.numeric(cores))
  doSNOW::registerDoSNOW(cl)
  #progress bar
  pb <- utils::txtProgressBar(min=1, max=length(configs$setting), style=3)
  progress <- function(n) utils::setTxtProgressBar(pb, n)
  opts <- list(progress=progress)
  # add namespace for variables inside the foreach.
  i <- NULL
  out.configs <- foreach::foreach(i = seq_along(configs$setting), 
                                  .packages=c("Kendall", "purrr", lib), 
                                  .options.snow=opts) %dopar% {
                                    setting <- configs$setting[[i]]
                                    PEcAn.uncertainty::write.ensemble.configs(
                                      defaults = setting$pfts,
                                      ensemble.samples = configs$ensemble.samples,
                                      settings = setting,
                                      model = setting$model$type,
                                      write.to.db = setting$database$bety$write,
                                      restart = configs$restart.list[[i]],
                                      rename = TRUE
                                    )
                                  }
  # stop parallel.
  parallel::stopCluster(cl)
  foreach::registerDoSEQ()
  # save outputs.
  save(out.configs, file = file.path(folder.path, "out.configs.Rdata"))
}

#' @description This function realizes the parallel `met.split` function.
#' @title met.split.fun
#' @param folder.path Character: physical path to which the job file is located.
#' @param cores numeric: number of requested CPUs.
#' @param lib Character: extra required library.
#' @author Dongchen Zhang.
#' @importFrom foreach %dopar%
met.split.fun <- function(folder.path, cores, lib) {
  configs <- readRDS(file.path(folder.path, "configs.rds"))
  # foreach.
  cl <- parallel::makeCluster(as.numeric(cores))
  doSNOW::registerDoSNOW(cl)
  #progress bar
  pb <- utils::txtProgressBar(min=1, max=length(configs$config.settings), style=3)
  progress <- function(n) utils::setTxtProgressBar(pb, n)
  opts <- list(progress=progress)
  # add namespace for variables inside the foreach.
  k <- NULL
  splits <- foreach::foreach(k = seq_along(configs$config.settings), 
                             .packages=c("Kendall", "purrr", "lubridate", lib), 
                             .options.snow=opts) %dopar% {
                               inputs.split <- list()
                               for (i in seq_len(configs$nens)) {
                                 #---------------- model specific split inputs
                                 ### model specific split inputs
                                 inputs.split$samples[i] <- do.call(configs$my.split_inputs, 
                                                                    args = list(settings = configs$config.settings[[k]], 
                                                                                start.time = configs$start.time, 
                                                                                stop.time = configs$stop.time, 
                                                                                inputs = configs$inputs[[k]]$samples[[i]]))
                               }
                               inputs.split
                             }
  parallel::stopCluster(cl)
  foreach::registerDoSEQ()
  save(splits, file = file.path(folder.path, "splits.Rdata"))
}

#' @description This function realizes the parallel `read.restart` function.
#' @title read.fun.
#' @param folder.path Character: physical path to which the job file is located.
#' @param cores numeric: number of requested CPUs.
#' @param lib Character: extra required library.
#' @author Dongchen Zhang.
#' @importFrom foreach %dopar%
read.fun <- function(folder.path, cores, lib) {
  configs <- readRDS(file.path(folder.path, "configs.rds"))
  # foreach.
  cl <- parallel::makeCluster(as.numeric(cores))
  doSNOW::registerDoSNOW(cl)
  #progress bar
  pb <- utils::txtProgressBar(min=1, max=length(configs$out.configs), style=3)
  progress <- function(n) utils::setTxtProgressBar(pb, n)
  opts <- list(progress=progress)
  # add namespace for variables inside the foreach.
  k <- NULL
  reads <- foreach::foreach(k = seq_along(configs$out.configs), 
                            .packages=c("Kendall", "purrr", lib), 
                            .options.snow=opts) %dopar% {
                              X_tmp <- vector("list", 2)
                              for (i in seq_len(configs$nens)) {
                                X_tmp[[i]] <- do.call(configs$my.read_restart,
                                                      args = list(
                                                        outdir = configs$outdir,
                                                        runid = configs$out.configs[[k]]$runs$id[i] %>% as.character(),
                                                        stop.time = configs$stop.time,
                                                        var.names = configs$var.names,
                                                        params = configs$new.params[[k]][[i]]))
                              }
                              return(X_tmp)
                            }
  parallel::stopCluster(cl)
  foreach::registerDoSEQ()
  save(reads, file = file.path(folder.path, "reads.Rdata"))
}

#' @description This function realizes the parallel `unlink` function.
#' @title rm.files
#' @param folder.path Character: physical path to which the job file is located.
#' @param cores Numeric: number of requested CPUs.
#' @param only.nc Boolean: determine if we just want to remove NC files.
#' @author Dongchen Zhang.
#' @importFrom foreach %dopar%
rm.files <- function(folder.path, cores, only.nc) {
  only.nc <- as.logical(only.nc)
  folder.runs <- readRDS(file.path(folder.path, "runs.rds"))
  # foreach.
  cl <- parallel::makeCluster(as.numeric(cores))
  doSNOW::registerDoSNOW(cl)
  #progress bar
  pb <- utils::txtProgressBar(min=1, max=length(folder.runs), style=3)
  progress <- function(n) utils::setTxtProgressBar(pb, n)
  opts <- list(progress=progress)
  # add namespace for variables inside the foreach.
  file <- NULL
  if (only.nc) {
    test = foreach::foreach(file = folder.runs, 
                            .packages=c("Kendall"), 
                            .options.snow=opts) %dopar% {
                              unlink(list.files(file, "*.nc", recursive = TRUE, full.names = TRUE))}
  } else {
    test = foreach::foreach(file = folder.runs, 
                            .packages=c("Kendall"), 
                            .options.snow=opts) %dopar% {
                              unlink(file, recursive = T)
                              unlink(gsub(pattern = "out", replacement = "run", x = file, fixed = T), recursive = T)}
  }
  parallel::stopCluster(cl)
  foreach::registerDoSEQ()
  writeLines("finished",con = file.path(folder.path, "result.txt"))
}

#' @description This function realizes the parallel model execution.
#' @title parallel.job.execution
#' @param folder.path Character: physical path to which the job file is located.
#' @param cores numeric: number of requested CPUs.
#' @author Dongchen Zhang.
#' @importFrom foreach %dopar%
parallel.job.execution <- function(folder.path, cores) {
  folder.runs <- readRDS(file.path(folder.path, "runs.rds"))
  # foreach.
  cl <- parallel::makeCluster(as.numeric(cores))
  doSNOW::registerDoSNOW(cl)
  pb <- utils::txtProgressBar(min=1, max=length(folder.runs), style=3)
  progress <- function(n) utils::setTxtProgressBar(pb, n)
  opts <- list(progress=progress)
  # add namespace for variables inside the foreach.
  folder <- NULL
  verb <- foreach::foreach(folder = folder.runs, 
                           .packages=c("Kendall"), 
                           .options.snow=opts) %dopar% {
                             job.path <- file.path(folder, "job.sh")
                             cmd.temp <- 'cd \"@JOBPATH@\";./job.sh'
                             cmd <- gsub(pattern = "@JOBPATH@", replacement = folder, x = cmd.temp)
                             out <- system(cmd, intern = TRUE)
                           }
  parallel::stopCluster(cl)
  foreach::registerDoSEQ()
  writeLines("finished",con = file.path(folder.path, "result.txt"))
}

##' This function can help to execute `foreach` parallel MCMC sampling given generated MCMC configuration lists.
##' @title qsub_analysis
##' @param folder.path character: path where the `block.Rdata` file is stored.
##' @param cores numeric: number of cpus used for parallel computaion. Default is NULL.
##' @author Dongchen Zhang.
##' @importFrom foreach %dopar%
qsub_analysis <- function(folder.path, cores) {
  # load file.
  blocks <- readRDS(file.path(folder.path, "block.rds"))
  # initialize parallel.
  cl <- parallel::makeCluster(as.numeric(cores))
  doSNOW::registerDoSNOW(cl)
  # progress bar
  pb <- utils::txtProgressBar(min=1, max=length(blocks), style=3)
  progress <- function(n) utils::setTxtProgressBar(pb, n)
  opts <- list(progress=progress)
  # parallel computation.
  # add namespace for variables inside the foreach.
  l <- NULL
  results <- foreach::foreach(l = blocks, 
                              .packages=c("Kendall", 
                                          "purrr", 
                                          "nimble",
                                          "PEcAnAssimSequential"), 
                              .options.snow=opts) %dopar% {
                                MCMC_block_function(l)
                              }
  # wrap results.
  parallel::stopCluster(cl)
  writeLines("finished",con = file.path(folder.path, "result.txt"))
  save(results, file = file.path(folder.path, "results.Rdata"))
}