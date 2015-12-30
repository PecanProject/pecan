#' @name invert.auto
#' @title Inversion with automatic convergence checking
#' @details Performs an inversion via the `invert.custom` function with 
#' multiple chains and automatic convergence checking. Convergence checks are 
#' performed using the multivariate Gelman-Rubin diagnostic.
#' @param observed Matrix of observed values. Must line up with output of 
#' 'model'.
#' @param settings R list object containing the following elements:
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
#' adj_min Minimum threshold for rescaling Jump standard deviation.  Default = 
#' 0.1.
#' 
#' target Target acceptance rate. Default=0.234
#' 
#' do.lsq Perform least squares optimization first (see `invert.lsq`), and use 
#' outputs to initialize Metropolis Hastings. This may improve mixing time, but 
#' risks getting caught in a local minimum.  Default=FALSE
#'
#' nchains Number of independent chains.
#' 
#' inits.function Function for randomly generating initial conditions.
#' burnin Number of samples to burn-in before computing Gelman 
#' Diagnostic. Default = 0.8 * ngibbs.
#'
#' n.tries Number of attempted runs before giving up. Default = 5
#'
#' do.lsq.first Initialize using least-squares optimization on first 
#' try. Default = FALSE.
#'
#' do.lsq.after Number of tries before starting initialization using 
#' least-squares optimization. Default = TRUE.
#'
#' @param return.samples Include full samples list in output. Default = TRUE.
#' target.adj Amount by which to adjust target acceptance rate every 
#' attempt. Default = 0.8
#' @param save.samples Filename for saving samples after each iteration. If 
#' 'NULL', do not save samples. Default = NULL.
#' @return List of "results" (summary statistics and Gelman Diagnostic) and 
#' "samples"(mcmc.list object, or "NA" if return.samples=FALSE)

invert.auto <- function(observed, settings, return.samples=TRUE, save.samples=NULL, quiet=FALSE){
    n.tries <- settings$n.tries
    nchains <- settings$nchains
    inits.function <- settings$inits.function
    settings$do.lsq <- settings$do.lsq.first
    try.again <- TRUE
    i.try <- 1
    while(try.again & i.try <= n.tries){
        print(sprintf("Attempt %d of %d", i.try, n.tries))
        samps.list <- list()
        for(chain in 1:nchains){
            print(sprintf("Chain %d of %d", chain, nchains))
            settings$inits <- inits.function() 
            samps.list[[chain]] <- invert.custom(observed=observed, settings=settings, quiet=quiet)
        }
        if(!is.null(save.samples)) save(samps.list, file=save.samples)
        # Check for convergence. Repeat if necessary.
        samps.list.bt <- lapply(samps.list, burnin.thin, burnin=burnin, thin=1)
        smcmc <- as.mcmc.list(lapply(samps.list.bt, as.mcmc))
        gd <- try(gelman.diag(smcmc, autoburnin=FALSE))
        if(is.character(gd)) {
            i.try <- i.try + 1
            warning("Could not calculate Gelman diag. Trying again")
            next
        } else {
            gdmp <- gd$mpsrf
            if(gdmp < 1.1){
                msg <- sprintf("Converged with Gelman diag = %.3f", gdmp)
                print(msg)
                try.again <- FALSE
                samps <- burnin.thin(do.call(rbind, samps.list.bt), burnin=0)
                results <- summary.simple(samps)
                results$gelman.diag <- gdmp
            } else {
                msg <- sprintf("Did not converge (Gelman Diag = %.3f). Trying again.", gdmp)
                print(msg)
                i.try <- i.try + 1
                settings$target <- settings$target * settings$target.adj
                if(i.try > settings$do.lsq.after) settings$do.lsq <- TRUE
            }
        }
    }
    if(return.samples) samples <- as.mcmc.list(lapply(samps.list, as.mcmc))
    else samples <- NA
    if((i.try >= n.tries) & try.again){
        warning("Convergence was not achieved. Returning results as 'NA'.")
        results <- NA
    }
    out <- list(results=results, samples=samples)
    return(out)
}
