#' @name invert.auto 
#' 
#' @title Inversion with automatic convergence checking
#' @details Performs an inversion via the `invert.custom` function with multiple chains and automatic convergence checking. 
#' Convergence checks are performed using the multivariate Gelman-Rubin diagnostic.
#' @param invert.options Parameters related to inversion.
#' Parameters specific to `invert.auto` are described here.
#' For the remaining parameters, see \code{\link{invert.custom}}.
#' 
#' model The model to be inverted. This should be an R function that takes 
#' `params` as input and returns one column of `observed` (nrows should be the 
#' same). Constants should be implicitly included here.
#'
#' nchains Number of independent chains.
#' 
#' inits.function Function for randomly generating initial conditions.
#' 
#' ngibbs.max Maximum number of total iterations (per chain). DEFAULT = 5e6
#'
#' ngibbs.min Minimum number of total iterations (per chain). DEFAULT = 5000.
#'
#' ngibbs.step Number of iterations between convergence checks. Default = 1000.
#'
#' run_first Function to run before running sampling. Takes parallel inputs 
#' list containing runID, initial values, and resume (NULL) as an argument.
#'
#' @param return.samples Include full samples list in output. Default = TRUE.
#' @param save.samples Save samples to file as the inversion proceeds (useful for debugging).
#' If NULL, do not save samples. Default = NULL.
#' @param parallel Logical. Whether or not to run multiple chains in parallel on multiple cores (default = TRUE).
#' @param parallel.cores Number of cores to use for parallelization. If NULL (default), allocate one fewer than detected number of cores.
#' @param parallel.output Filename (or '' for stdout) for printing parallel outputs. Use with caution. Default = '/dev/null'.
#' @inheritParams invert.custom
#' @return List of "results" (summary statistics) and "samples" (mcmc.list object, or "NA" if return.samples=FALSE)
#' @export

invert.auto <- function(observed, invert.options,
                        return.samples = TRUE,
                        save.samples = NULL, 
                        quiet=FALSE,
                        parallel=TRUE,
                        parallel.cores=NULL,
                        parallel.output = '/dev/null') {
  if (parallel == TRUE) {
    testForPackage("parallel")
  } else {
    message("Running in serial mode. Better performance can be achived with `parallel=TRUE`.")
  }
  ngibbs.max <- invert.options$ngibbs.max
  ngibbs.step <- invert.options$ngibbs.step
  nchains <- invert.options$nchains
  inits.function <- invert.options$inits.function
  if (is.null(invert.options$do.lsq)) { 
    invert.options$do.lsq <- FALSE 
  }
  if (invert.options$do.lsq) {
    testForPackage("minpack.lm")
  }
  invert.options$ngibbs <- invert.options$ngibbs.min
  max_iter_converge_check <- invert.options$max_iter_converge_check
  if (is.null(max_iter_converge_check)) {
    max_iter_converge_check <- 15000
  }
  
  # Set up cluster for parallel execution
  if (parallel) {
    ## Create cluster
    maxcores <- parallel::detectCores()
    if (is.null(parallel.cores)) {
      parallel.cores <- maxcores - 1
    } else {
      if (!is.numeric(parallel.cores) | parallel.cores %% 1 != 0) {
        stop("Invalid argument to 'parallel.cores'. Must be integer or NULL")
      } else if (parallel.cores > maxcores) {
        warning(sprintf("Requested %1$d cores but only %2$d cores available. ", 
                        parallel.cores, maxcores), 
                "Using only available cores.")
        parallel.cores <- maxcores
      }
    }
    cl <- parallel::makeCluster(parallel.cores, "FORK", outfile = parallel.output)
    on.exit(parallel::stopCluster(cl))
    message(sprintf("Running %d chains in parallel. ", nchains), 
            "Progress bar unavailable")
  }
  
  # Create inversion function to be passed to parLapply
  invert.function <- function(x) {
    invert.options$inits <- x$inits
    invert.options$resume <- x$resume
    samps <- invert.custom(observed = observed,
                           invert.options = invert.options, 
                           quiet = quiet,
                           return.resume = TRUE,
                           runID = x$runID)
    return(samps)
  }
  
  runID_list <- seq_len(nchains)
  inputs <- list()
  for (i in seq_len(nchains)) { 
    inputs[[i]] <- list(runID = runID_list[i],
                        inits = inits.function(),
                        resume = NULL)
  }

  # Do initialization step if provided
  if (!is.null(invert.options$run_first)) {
    if (parallel) {
      first <- parallel::parLapply(cl, inputs, invert.options$run_first)
    } else {
      first <- list()
      for (i in seq_len(nchains)) {
        first[[i]] <- invert.options$run_first(inputs[[i]])
      }
    } 
  }

  # Begin inversion
  invert.options$ngibbs <- invert.options$ngibbs.min
  if (parallel) {
    output.list <- parallel::parLapply(cl, inputs, invert.function)
  } else {
    output.list <- list()
    for (i in seq_along(inputs)) {
      print(sprintf("Running chain %d of %d", i, nchains))
      output.list[[i]] <- invert.function(inputs[[i]])
    }
  }

  i.ngibbs <- invert.options$ngibbs.min
  samps.list <- lapply(output.list, "[[", "results")
  resume <- lapply(output.list, "[[", "resume")
  if (!is.null(save.samples)) {
    saveRDS(list(resume = resume, samps.list = samps.list),
            file = save.samples)
  }
  process <- process_samples(samps.list, max_iter_converge_check)
  finished <- process$finished

  if (finished) {
    out <- process[c('results', 'samples')]
  }

  # Loop until convergence (skipped if finished == TRUE)
  while (!finished & i.ngibbs < ngibbs.max) {
    if (!quiet) {
      message(sprintf("Running iterations %d to %d", i.ngibbs,
                      i.ngibbs + ngibbs.step))
    }
    inits <- lapply(samps.list, getLastRow)
    inputs <- list()
    for (i in seq_len(nchains)) {
      inputs[[i]] <- list(runID = runID_list[i],
                          inits = inits[[i]],
                          resume = resume[[i]])
    }
    invert.options$ngibbs <- ngibbs.step
    if (parallel) {
      output.list <- parallel::parLapply(cl, inputs, invert.function)
    } else {
      output.list <- list()
      for (i in seq_along(inputs)) {
        message(sprintf('Running chain %d of %d', i, nchains))
        output.list[[i]] <- invert.function(inputs[[i]])
      }
    }
    i.ngibbs <- i.ngibbs + ngibbs.step
    samps.list.current <- lapply(output.list, '[[', 'results')
    resume <- lapply(output.list, '[[', 'resume')

    samps.list <- combineChains(samps.list, samps.list.current)
    if (!is.null(save.samples)) {
      saveRDS(list(resume = resume, samps.list = samps.list),
              file = save.samples)
    }
    process <- process_samples(samps.list, max_iter_converge_check)
    finished <- process$finished

    if (finished) {
      out <- process[c('results', 'samples')]
    }

  }
  if (i.ngibbs > ngibbs.max & !finished) {
    warning("Convergence was not achieved, and max iterations exceeded. ",
            "Returning results as 'NA'.")
    out <- list(results = NA, 
                samples = PEcAn.assim.batch::makeMCMCList(samps.list))
  }
  if (!return.samples) {
    out$samples <- c(`Samples not returned` = NA)
  }
  return(out)
} # invert.auto

getLastRow <- function(samps, exclude.cols = ncol(samps)) {
  cols <- seq_len(ncol(samps))
  cols <- cols[-exclude.cols]
  last_row <- samps[nrow(samps), cols]
  return(last_row)
} # getLastRow

combineChains <- function(samps1, samps2) {
  stopifnot(length(samps1) == length(samps2))
  nchains <- length(samps1)
  sampsfinal <- list()
  for (i in seq_len(nchains)) {
    sampsfinal[[i]] <- rbind(samps1[[i]], samps2[[i]])
  }
  stopifnot(length(sampsfinal) == length(samps1))
  return(sampsfinal)
} # combineChains

process_samples <- function(samps.list, max_iter_converge_check) {
  smcmc <- PEcAn.assim.batch::makeMCMCList(samps.list)
  nsamp <- coda::niter(smcmc)
  nburn <- min(floor(nsamp/2), max_iter_converge_check)
  smcmc_sub <- window(smcmc, start = nburn)
  check_initial <- check.convergence(smcmc_sub, autoburnin = FALSE)
  if (check_initial$error) {
    warning("Could not calculate Gelman diag. Assuming no convergence.")
    return(list(finished = FALSE))
  }
  if (!check_initial$converged) {
    message("Convergence was not achieved. Continuing sampling.")
    return(list(finished = FALSE))
  } else {
    message("Passed initial convergence check. ",
            "Attempting automatic burnin.")
  }
  burn <- PEcAn.assim.batch::autoburnin(smcmc, return.burnin = TRUE)
  if (burn$burnin == 1) {
    message("Robust convergence check in autoburnin failed. ",
            "Resuming sampling.")
    return(list(finished = FALSE))
  } else {
    message("Converged after ", coda::niter(smcmc), "iterations.\n",
            "Burnin = ", burn$burnin)
  }
  results <- summary_simple(do.call(rbind, burn$samples))
  return(list(results = results, samples = smcmc, finished = TRUE))
}

