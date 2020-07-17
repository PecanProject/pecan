#' Inversion with automatic convergence checking
#'
#' @details Performs an inversion via the \code{invert.custom} function with
#' multiple chains and automatic convergence checking.  Convergence checks are
#' performed using the multivariate Gelman-Rubin diagnostic.
#' @param invert.options Parameters related to inversion.
#' @param return.samples Include full samples list in output. Default = `TRUE.`
#' @param save.samples Save samples to file as the inversion proceeds (useful
#' for debugging). If `NULL`, do not save samples. Default = `NULL`.
#' @param parallel Logical. Whether or not to run multiple chains in parallel
#' on multiple cores (default = `TRUE`).
#' @param parallel.cores Number of cores to use for parallelization. If
#' `NULL` (default), allocate one fewer than detected number of cores.
#' @param parallel.output Filename (or '' for stdout) for printing parallel
#' outputs. Use with caution. Default = `'/dev/null'`.
#' @inheritParams invert.custom
#' 
#' @details
#' Parameters specific to `invert.auto` are described here.
#' For the remaining parameters, see [invert.custom()].
#' 
#' * `model` -- The model to be inverted. This should be an R function that
#' takes \code{params} as input and returns one column of \code{observed}
#' (nrows should be the same). Constants should be implicitly included here.
#'
#' * `nchains` -- Number of independent chains.
#' 
#' * `inits.function` -- Function for generating initial conditions.
#' 
#' * `ngibbs.max` -- Maximum number of total iterations (per chain). DEFAULT = 5e6
#'
#' * `ngibbs.min` -- Minimum number of total iterations (per chain). DEFAULT = 5000.
#'
#' * `ngibbs.step` -- Number of iterations between convergence checks. Default = 1000.
#'
#' * `run_first` -- Function to run before running sampling. Takes parallel
#' inputs list containing runID, initial values, and resume (NULL) as an
#' argument.
#'
#' * `calculate.burnin` -- If `TRUE`, use `PEcAn.assim.batch::autoburin`
#' function to calculate burnin. Otherwise, assume burnin is `min(niter/2,
#' iter_conv_check)`.
#'
#' * `threshold` -- Maximum value of the Gelman-Rubin diagnostic for
#' determining convergence. Default = 1.1
#'
#' @return List including `results` (summary statistics), `samples`
#' (`mcmc.list` object, or `NA` if `return.samples=FALSE`), and other
#' information.
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
  if (is.null(ngibbs.max)) {
    ngibbs.max <- 1e6
    message("ngibbs.max not provided. ",
            "Setting default to ", ngibbs.max)
  }
  ngibbs.min <- invert.options$ngibbs.min
  if (is.null(ngibbs.min)) {
    ngibbs.min <- 5000
    message("ngibbs.min not provided. ",
            "Setting default to ", ngibbs.min)
  }
  ngibbs.step <- invert.options$ngibbs.step
  if (is.null(ngibbs.step)) {
    ngibbs.step <- 1000
    message("ngibbs.step not provided. ",
            "Setting default to ", ngibbs.step)
  }
  nchains <- invert.options$nchains
  if (is.null(nchains)) {
    nchains <- 3
    message("nchains not provided. ",
            "Setting default to ", nchains)
  }
  inits.function <- invert.options$inits.function
  if (is.null(inits.function)) {
    stop("invert.options$inits.function is required but missing.")
  }
  if (is.null(invert.options$do.lsq)) {
    invert.options$do.lsq <- FALSE
    message("do.lsq not provided. ",
            "Setting default to ", invert.options$do.lsq)
  }
  if (invert.options$do.lsq) {
    testForPackage("minpack.lm")
  }
  iter_conv_check <- invert.options$iter_conv_check
  if (is.null(iter_conv_check)) {
    iter_conv_check <- 15000
    message("iter_conv_check not provided. ",
            "Setting default to ", iter_conv_check)
  }
  threshold <- invert.options$threshold
  if (is.null(threshold)) {
    threshold <- 1.1
    message("threshold not provided. ",
            "Setting default to ", threshold)
  }
  calculate.burnin <- invert.options$calculate.burnin
  if (is.null(calculate.burnin)) {
    calculate.burnin <- TRUE
    message("calculate.burnin not provided. ",
            "Setting default to ", calculate.burnin)
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
    on.exit(parallel::stopCluster(cl), add = TRUE)

    # Initialize random seeds on cluster.
    # Otherwise, chains may start on same seed and end up identical.
    parallel::clusterSetRNGStream(cl)

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
  invert.options$ngibbs <- ngibbs.min
  i.ngibbs <- ngibbs.min
  if (parallel) {
    output.list <- parallel::parLapply(cl, inputs, invert.function)
  } else {
    output.list <- list()
    for (i in seq_along(inputs)) {
      print(sprintf("Running chain %d of %d", i, nchains))
      output.list[[i]] <- invert.function(inputs[[i]])
    }
  }

  resume <- lapply(output.list, '[[', 'resume')
  out <- process_output(output.list = output.list,
                        iter_conv_check = iter_conv_check,
                        save.samples = save.samples,
                        threshold = threshold,
                        calculate.burnin = calculate.burnin)

  # Loop until convergence (skipped if finished == TRUE)
  invert.options$ngibbs <- ngibbs.step
  while (!out$finished & i.ngibbs < ngibbs.max) {
    if (!quiet) {
      message(sprintf("Running iterations %d to %d", i.ngibbs,
                      i.ngibbs + ngibbs.step))
    }
    inits <- lapply(out$samples, getLastRow)
    inputs <- list()
    for (i in seq_len(nchains)) {
      inputs[[i]] <- list(runID = runID_list[i],
                          inits = inits[[i]],
                          resume = resume[[i]])
    }
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
    resume <- lapply(output.list, '[[', 'resume')
    out <- process_output(output.list = output.list,
                          prev_out = out,
                          iter_conv_check = iter_conv_check,
                          save.samples = save.samples,
                          threshold = threshold,
                          calculate.burnin = calculate.burnin)
  }

  if (i.ngibbs > ngibbs.max & !out$finished) {
    warning("Convergence was not achieved, and max iterations exceeded. ",
            "Returning results as 'NA'.")
  }
  if (!return.samples) {
    out$samples <- c('Samples not returned' = NA)
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
  out <- PEcAn.assim.batch::makeMCMCList(sampsfinal)
  return(out)
} # combineChains

process_output <- function(output.list,
                           prev_out = NULL,
                           iter_conv_check,
                           save.samples,
                           threshold,
                           calculate.burnin) {

  samples.current <- lapply(output.list, "[[", "results")
  deviance_list.current <- lapply(output.list, "[[", "deviance")
  n_eff_list.current <- lapply(output.list, "[[", "n_eff")
  rm(output.list)

  out <- list()

  if (is.null(prev_out)) {
    out$samples <- PEcAn.assim.batch::makeMCMCList(samples.current)
    out$deviance_list <- deviance_list.current
    out$n_eff_list <- n_eff_list.current
  } else {
    out$samples <- combineChains(prev_out$samples, samples.current)
    out$deviance_list <- mapply(c, prev_out$deviance_list,
                                deviance_list.current, SIMPLIFY = F)
    out$n_eff_list <- mapply(c, prev_out$n_eff_list, n_eff_list.current,
                             SIMPLIFY = F)
  }
  rm(prev_out)

  if (!is.null(save.samples)) {
    saveRDS(out, file = save.samples)
  }

  out$nsamp <- coda::niter(out$samples)
  nburn <- min(floor(out$nsamp/2), iter_conv_check)
  burned_samples <- window(out$samples, start = nburn)
  check_initial <- check.convergence(burned_samples,
                                     threshold = threshold,
                                     autoburnin = FALSE)
  if (check_initial$error) {
    warning("Could not calculate Gelman diag. Assuming no convergence.")
    out$finished <- FALSE
    return(out)
  }
  if (!check_initial$converged) {
    message("Convergence was not achieved. Continuing sampling.")
    out$finished <- FALSE
    return(out)
  } else {
    message("Passed initial convergence check.")
  }
  if (calculate.burnin) {
    burn <- PEcAn.assim.batch::autoburnin(out$samples, return.burnin = TRUE, method = 'gelman.plot')
    out$burnin <- burn$burnin
    if (out$burnin == 1) {
      message("Robust convergence check in autoburnin failed. ",
              "Resuming sampling.")
      out$finished <- FALSE
      return(out)
    } else {
      message("Converged after ", out$nsamp, "iterations.")
      out$results <- summary_simple(do.call(rbind, burn$samples))
    }
  } else {
    message("Skipping robust convergece check (autoburnin) because ",
            "calculate.burnin == FALSE.")
    out$burnin <- nburn
    out$results <- summary_simple(do.call(rbind, burned_samples))
  }
  message("Burnin = ", out$burnin)
  out$finished <- TRUE
  return(out)
}
