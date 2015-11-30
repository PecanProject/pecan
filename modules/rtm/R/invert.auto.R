#' @name invert.auto
#' @title Inversion with automatic convergence checking
#' @details Performs an inversion via the `invert.custom` function with 
#' multiple chains and automatic convergence checking. Convergence checks are 
#' performed using the multivariate Gelman-Rubin diagnostic.
#' @param observed Matrix of observed values. Must line up with output of 
#' 'model'.
#' @param model Function that calls the model to be inverted. Must take a 
#' single argument containing a vector of inputs.
#' @param ngibbs Number of iterations.
#' @param nchains Number of independent chains.
#' @param prior.function Function called to calculate the prior.
#' @param inits.function Function for randomly generating initial conditions.
#' @param param.mins Parameter strict lower bounds.
#' @param burnin Number of samples to burn-in before computing Gelman 
#' Diagnostic. Default = 0.8 * ngibbs.
#' @param n.tries Number of attempted runs before giving up. Default = 5
#' @param return.samples Include full samples list in output. Default = TRUE.
#' @param target.adj Amount by which to adjust target acceptance rate every 
#' attempt. Default = 0.8
#' @param do.lsq.first Initialize using least-squares optimization on first 
#' try. Default = FALSE.
#' @param do.lsq.after Number of tries before starting initialization using 
#' least-squares optimization. Default = TRUE.
#' @param save.samples Filename for saving samples after each iteration. If 
#' 'NULL', do not save samples. Default = NULL.
#' @return List of "results" (summary statistics and Gelman Diagnostic) and 
#' "samples"(mcmc.list object, or "NA" if return.samples=FALSE)

invert.auto <- function(observed,
                        model,
                        ngibbs,
                        nchains,
                        prior.function,
                        inits.function,
                        param.mins,
                        burnin=0.8*ngibbs,
                        n.tries=5,
                        return.samples=TRUE,
                        target = 0.234,
                        target.adj=0.8,
                        do.lsq.first = FALSE,
                        do.lsq.after = 3,
                        save.samples=NULL,
                        ...){
    do.lsq <- do.lsq.first
    try.again <- TRUE
    i.try <- 1
    while(try.again & i.try <= n.tries){
        print(sprintf("Attempt %d of %d", i.try, n.tries))
        samps.list <- list()
        for(chain in 1:nchains){
            print(sprintf("Chain %d of %d", chain, nchains))
            inits <- inits.function() 
            samps.list[[chain]] <- invert.custom(observed=observed, 
                                                 inits=inits,
                                                 ngibbs=ngibbs,
                                                 prior.function=prior.function,
                                                 param.mins=param.mins,
                                                 model=model,
                                                 do.lsq=do.lsq,
                                                 ...)
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
                target <- target * target.adj
                if(i.try > do.lsq.after) do.lsq <- TRUE
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
