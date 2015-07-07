#' @name invert.fast.re
#' @title Bayesian inversion with random effects
#' @author Alexey Shiklomanov
#' @details Performs a Bayesian inversion of a Radiative
#' Transfer model with individual random effects. Sampling
#' is performed using an adaptive Metropolis-Hastings
#' algorithm operating independently on each parameter. See
#' also: `invert.fast`.
#' @param modname Name of the model to invert (character).
#' Refer to `model.list`
#' @param observed Observed reflectance. Can be a vector,
#' matrix, or data.frame; BUT NOTE: all are coerced to
#' matrix via `as.matrix`.
#' @param inits Named vector of parameters to invert. Names
#' are required!
#' @param cons Numeric vector of constants. Names are
#' required!
#' @param rand Numeric matrix of initial values for random
#' effects, with dimensions (npars x nspec), where npars is
#' the number of parameters and nspec is the number of
#' spectra.
#' @param pmu Numeric vector of prior means for inversion
#' parameters. Must line up with `inits`
#' @param psd Numeric vector of prior standard deviations
#' for inversion parameters. Must line up with `inits`.
#' @param plog Logical vector. Whether or not to use
#' lognormal distribution for the prior. NOTE: `pmu` and `psd`
#' are the distribution parameters, NOT the distribution's
#' actual mean and standard deviation.
#' @param minp Numeric vector of minimum values for parameters.
#' @param ngibbs Number of iterations for MCMC 
#' @return Matrix (ngibbs x (npars*(nspec+2)+1)) of MCMC
#' samples of parameters.

invert.fast.re <- function(modname, observed, inits, rand, cons, 
                   pmu, psd, plog, minp, ngibbs){
# Get model code number
    data(model.list)
    setkey(model.list, modname)
    model.set <- model.list[modname]
    if(all(is.na(model.set[,-1,with=FALSE]))){
        stop(sprintf("Error: Model '%s' not found", modname))
    }
    modcode <- as.integer(model.set$modcode)
    print(sprintf("Model: %s; Code: %d", model.set$fullname, modcode))

# Setup initial conditions and constants
    names.all <- unlist(strsplit(model.set$par.names, " "))
    names.inits <- names(inits)
    stopifnot(!is.null(names.inits))
    npars <- length(inits)
    ipars <- match(names.inits, names.all)
    if(length(cons) > 0){
        names.cons <- names(cons)
        stopifnot(!is.null(names.cons))
        ncons <- length(cons)
        icons <- match(names.cons, names.all)
    } else {
        cons <- numeric(0)
        ncons <- as.integer(0)
        icons <- numeric(0)
    }

# Setup random effects
    names.rand <- rownames(rand)
    stopifnot(!is.null(names.rand) && 
              length(names.rand) == length(inits))
    ord.rand <- match(names.rand, names.inits)
    rand <- rand[ord.rand,]

# Force correct types for other parameters
    observed <- as.matrix(observed)
    nspec <- ncol(observed)
    ngibbs <- as.integer(ngibbs)
    results <- matrix(0, ngibbs, npars*(nspec+2)+1)
    seed <- round(1e8 * runif(100))
    seed <- as.integer(seed)

# Group parameters and execute
    in.list <- list("invert_re", observed, nspec, modcode,
                    inits, npars, ipars, rand, 
                    cons, ncons, icons,
                    pmu, psd, plog, minp, ngibbs, results, seed)
    t1 <- proc.time()
    out.list <- do.call(.Fortran, in.list)
    t2 <- proc.time()
    print(t2 - t1)
    outmat <- out.list[[length(out.list)-1]]
    colnames(outmat) <- c(names.inits,
                          sprintf("RE_%s_%d", names.rand, rep(1:nspec, each=npars)),
                          sprintf("Tau_%s", names.rand),
                          "rsd")
    return(outmat)
}
