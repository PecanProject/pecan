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
  
  convergenceCheck <- function(smcmc) {
    out <- check.convergence(smcmc, autoburnin = TRUE, verbose = !quiet)
    return(out)
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
        warning(sprintf("Requested %1$d cores but only %2$d cores available. Using only %2$d cores.", 
                        parallel.cores, maxcores))
        parallel.cores <- maxcores
      }
    }
    cl <- parallel::makeCluster(parallel.cores, "FORK", outfile = parallel.output)
    on.exit(parallel::stopCluster(cl))
    print(sprintf("Running %d chains in parallel. Progress bar unavailable", nchains))
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
  # Check for convergence
  smcmc <- PEcAn.assim.batch::makeMCMCList(samps.list)
  conv.check <- convergenceCheck(smcmc)
  if (conv.check$error) {
    warning("Could not calculate Gelman diag. Assuming no convergence.")
    conv.check$converged <- FALSE
  }
  if (conv.check$converged) {
    # Done
    message("Post-process")
    out <- postProcess(i.ngibbs, samps.list)
  } else {
    # Loop until convergence
    continue <- TRUE
    while (continue & i.ngibbs < ngibbs.max) {
      if (!quiet) 
        print(sprintf("Running iterations %d to %d", i.ngibbs, i.ngibbs + ngibbs.step))
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
          print(sprintf("Running chain %d of %d", i, nchains))
          output.list[[i]] <- invert.function(inputs[[i]])
        }
      }
      i.ngibbs <- i.ngibbs + ngibbs.step
      samps.list.current <- lapply(output.list, "[[", 'results')
      resume <- lapply(output.list, "[[", 'resume')

      samps.list <- combineChains(samps.list, samps.list.current)
      if (!is.null(save.samples)) {
        saveRDS(list(resume = resume, samps.list = samps.list), file = save.samples)
      }
      smcmc <- PEcAn.assim.batch::makeMCMCList(samps.list)
      conv.check <- convergenceCheck(smcmc)
      
      # Check for convergence
      if (conv.check$error) {
        warning("Could not calculate Gelman diag. Assuming no convergence.")
        conv.check$converged <- FALSE
      }
      if (conv.check$converged) {
        # Done
        out <- postProcess(i.ngibbs, samps.list)
        continue <- FALSE
      } else {
        continue <- TRUE
      }
    }
    if (i.ngibbs > ngibbs.max & continue) {
      warning("Convergence was not achieved, and max iterations exceeded. Returning results as 'NA'.")
      out <- list(results = NA, 
                  samples = PEcAn.assim.batch::makeMCMCList(samps.list))
    }
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

postProcess <- function(i.ngibbs, samps.list) {
  print(sprintf("Converged after %d iterations", i.ngibbs))
  samps.out <- PEcAn.assim.batch::makeMCMCList(samps.list)
  # Calculate summary statistics
  samps.bt.out <- PEcAn.assim.batch::autoburnin(samps.out, return.burnin = TRUE)
  samps.bt <- samps.bt.out$samples
  print(paste("Burnin =", samps.bt.out$burnin))
  samps.combined <- do.call(rbind, samps.bt)
  results <- summary_simple(samps.combined)
  return(list(results = results, samples = samps.out))
} # postProcess
