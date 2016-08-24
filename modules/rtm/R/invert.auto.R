#' @name invert.auto 
#' 
#' @title Inversion with automatic convergence checking
#' @details Performs an inversion via the `invert.custom` function with multiple chains and automatic convergence checking. 
#' Convergence checks are performed using the multivariate Gelman-Rubin diagnostic.
#' @param observed Matrix of observed values. Must line up with output of 'model'.
#' @param invert.options R list object containing the following elements:
#' 
#' inits Vector of initial values of model parameters to be inverted.
#'
#' ngibbs Number of MCMC iterations
#'
#' prior.function Function for use as prior. Should take a vector of parameters 
#' as input and return a single value -- the sum of their log-densities -- as 
#' output.
#'
#' param.mins Vector of minimum values for inversion parameters
#'
#' model The model to be inverted. This should be an R function that takes 
#' `params` as input and returns one column of `observed` (nrows should be the 
#' same). Constants should be implicitly included here.
#'
#' adapt Number of steps for adapting covariance matrix (i.e. adapt every 'n' 
#' steps). Default=100
#' 
#' adj_min Minimum threshold for rescaling Jump standard deviation.  Default = 
#' 0.1.
#' 
#' target Target acceptance rate. Default=0.234, based on recommendation for 
#' multivariate block sampling in Haario et al. 2001
#' 
#' do.lsq Perform least squares optimization first (see `invert.lsq`), and use 
#' outputs to initialize Metropolis Hastings. This may improve mixing time, but 
#' risks getting caught in a local minimum.  Default=FALSE
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
#' @param return.samples Include full samples list in output. Default = TRUE.
#' @param save.samples Save samples to file as the inversion proceeds (useful for debugging).
#' If NULL, do not save samples. Default = NULL.
#' @param parallel Logical. Whether or not to run multiple chains in parallel on multiple cores (default = TRUE).
#' @param parallel.cores Number of cores to use for parallelization. If NULL (default), allocate one fewer than detected number of cores.
#' @return List of "results" (summary statistics and Gelman Diagnostic) and "samples" (mcmc.list object, or "NA" if return.samples=FALSE)

invert.auto <- 
    function(observed, invert.options, return.samples = TRUE, save.samples = NULL, 
             quiet=FALSE, parallel=TRUE, parallel.cores=NULL){
    library(coda)
    if (parallel == TRUE) {
        library(parallel)
    } else {
        warning("Running in serial mode is currently unsupported. Switching to parallel.")
        library(parallel)
        parallel <- TRUE
    }
    ngibbs.max <- invert.options$ngibbs.max
    ngibbs.step <- invert.options$ngibbs.step
    nchains <- invert.options$nchains
    inits.function <- invert.options$inits.function
    if(invert.options$do.lsq) library(minpack.lm)
    invert.options$ngibbs <- invert.options$ngibbs.min
    burnin <- invert.options$ngibbs * 0.8

    # Begin first set of runs
    ## Create cluster
    maxcores <- detectCores()
    if(is.null(parallel.cores)){
        cl <- makeCluster(maxcores - 1, "FORK")
    } else {
        if(!is.numeric(parallel.cores) | parallel.cores %% 1 != 0){
            stop("Invalid argument to 'parallel.cores'. Must be integer or NULL")
        } else if (parallel.cores > maxcores){
            warning(sprintf("Requested %1$d cores but only %2$d cores available. Using only %2$d cores.", 
                            parallel.cores, maxcores))
            parallel.cores <- maxcores
        }
        cl <- makeCluster(parallel.cores, "FORK")
    }
    print(sprintf("Running %d chains in parallel. Progress bar unavailable", nchains))

    # Create inversion function
    invert.function <- function(x){
        set.seed(x$seed)
        invert.options$inits <- x$inits
        invert.options$init.Jump <- x$init.Jump
        samps <- invert.custom(observed=observed, invert.options=invert.options, 
                               quiet=quiet, return.jump=TRUE, seed = x)
        return(samps)
    }
    seeds <- 1e8 * runif(nchains)
    inits <- lapply(1:nchains, function(x) inits.function())
    inputs <- list()
    for (i in 1:nchains) inputs[[i]] <- list(seed = seeds[i], inits = inits[[i]], jump = NULL)

    # Begin inversion
    invert.options$ngibbs <- invert.options$ngibbs.min
    output.list <- parLapply(cl, inputs, invert.function)
    if (!is.null(save.samples)) save(output.list, file = save.samples)
    samps.list <- lapply(output.list, "[[", 'results')
    jump.list <- lapply(output.list, "[[", 'jump')

    # Check for convergence
    smcmc <- makeMCMCList(samps.list)
    conv.check <- check.convergence(smcmc, autoburnin=TRUE, verbose = !quiet)
    if (conv.check$error) {
        warning("Could not calculate Gelman diag. Assuming no convergence.")
        conv.check$converged <- FALSE
    }
    if (conv.check$converged) {
        # Done
        out <- postProcess(i.ngibbs, samps.list, conv.check)
    } else {
        # Loop until convergence
        continue <- TRUE
        i.ngibbs <- invert.options$ngibbs.min
        while (continue & i.ngibbs < ngibbs.max) {
            if (!quiet) print(sprintf("Running iterations %d to %d", i.ngibbs, i.ngibbs + ngibbs.step))
            seeds <- 1e8 * runif(nchains)
            inits <- lapply(samps.list, getLastRow)
            inputs <- list()
            for (i in 1:nchains) inputs[[i]] <- list(seed = seeds[i],
                                                     inits = inits[[i]],
                                                     jump = jump.list[[i]])
            invert.options$ngibbs <- ngibbs.step
            output.list <- parLapply(cl, inputs, invert.function)
            i.ngibbs <- i.ngibbs + ngibbs.step
            if (!is.null(save.samples)) save(output.list, samps.list, file = save.samples)
            samps.list.current <- lapply(output.list, "[[", 'results')
            samps.list <- combineChains(samps.list, samps.list.current)
            jump.list <- lapply(output.list, "[[", 'jump')
            
            # Check for convergence
            smcmc <- makeMCMCList(samps.list)
            conv.check <- check.convergence(smcmc, autoburnin=TRUE, verbose = !quiet)
            if (conv.check$error) {
                warning("Could not calculate Gelman diag. Assuming no convergence.")
                conv.check$converged <- FALSE
            }
            if (conv.check$converged) {
                # Done
                out <- postProcess(i.ngibbs, samps.list, conv.check)
                continue <- FALSE
            } else {
                continue <- TRUE
            }
        }
        if (i.ngibbs > ngibbs.max & continue) {
            warning("Convergence was not achieved. Returning results as 'NA'.")
            out$results <- NA
        }
    }
    if (!return.samples) out$samples <- c("Samples not returned" = NA)
    return(out)
}

getLastRow <- function(samps, exclude.cols = ncol(samps)){
    cols <- 1:ncol(samps)
    cols <- cols[-exclude.cols]
    last_row <- samps[nrow(samps), cols]
    return(last_row)
}

combineChains <- function(samps1, samps2){
    stopifnot(length(samps1) == length(samps2))
    nchains <- length(samps1)
    sampsfinal <- list()
    for (i in 1:nchains) sampsfinal[[i]] <- rbind(samps1[[1]], samps2[[i]])
    stopifnot(length(sampsfinal) == length(samps1))
    return(sampsfinal)
}

makeMCMCList <- function(samps) {
    samps.mcmc <- lapply(samps, mcmc)
    stopifnot(all(sapply(samps.mcmc, is.mcmc)))
    samps.mcmc.list <- mcmc.list(samps.mcmc)
    stopifnot(is.mcmc.list(samps.mcmc.list))
    return(samps.mcmc.list)
}

postProcess <- function(i.ngibbs, samps.list, conv.check) {
    print(sprintf("Converged after %d iterations with Gelman diag. %.4f",
                  i.ngibbs, conv.check$diagnostic))
    samps.out <- makeMCMCList(samps.list)
    # Calculate summary statistics
    samps.bt <- lapply(samps.list, burnin.thin)
    samps.combined <- do.call(rbind, samps.bt)
    results <- summary.simple(samps.combined)
    results$gelman.diag <- conv.check$diagnostic
    return(list(results = results, samples = samps.out))
}
