#' @name invert.fast
#' @title Bayesian RTM inversion: Fortran implementation
#' @author Alexey Shiklomanov
#' @details Performs a Bayesian inversion of a Radiative 
#'          Transfer Model. Sampling is performed using an 
#'          adaptive Metropolis-Hastings algorithm operating 
#'          independently on each parameter. The model takes 
#'          both paramters and constants as an argument -- 
#'          parameters (`inits`) are sampled and treated as 
#'          random (unknown) while constants (`cons`) are fixed. 
#'          Normal or log-normal priors are be specified by 
#'          passing a vector of means (`pmu`), standard 
#'          deviations (`psd`), and a logical of whether or not 
#'          to use the lognorml (`plog`). Minimum values for 
#'          the parameters must be given as a vector (`minp`).
#' @param modname Name of the model to invert (character). 
#'          Refer to `data/model.list.csv`
#' @param observed Observed reflectance. Can be a vector, matrix, 
#'          or data.frame; BUT NOTE: all are coerced via "as.matrix"
#' @param inits Named vector of parameters to invert. Names are required!
#' @param cons Named vector of constants. Names are required!
#' @param pmu Numeric vector of prior means for inversion parameters. 
#'          Must line up with `inits`
#' @param psd Numeric vector of prior standard deviations for inversion 
#'          parameters. Must line up with `inits`.
#' @param plog Logical vector. Whether or not to use lognormal 
#'          distribution for the prior. NOTE: `pmu` and `psd` are 
#'          the distribution parameters, NOT the distribution's actual 
#'          mean and standard deviation.
#' @param minp Numeric vector of minimum values for parameters.
#' @param ngibbs Number of iterations for MCMC 
#' @return Matrix (ngibbs x (length(inits) + 1)) of MCMC samples 
#'          of parameters.

invert.fast <- function(modname, observed, inits, cons, 
                   pmu, psd, plog, minp, ngibbs){
    data(model.list)
    setkey(model.list, modname)
    model.set <- model.list[modname]
    if(all(is.na(model.set[,-1,with=FALSE]))){
        stop(sprintf("Error: Model '%s' not found", modname))
    }
    modcode <- as.integer(model.set$modcode)
    print(sprintf("Model: %s; Code: %d", model.set$fullname, modcode))
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

    observed <- as.matrix(observed)
    nspec <- ncol(observed)

    ngibbs <- as.integer(ngibbs)
    results <- matrix(0, ngibbs, npars+1)
    seed <- round(1e8 * runif(100))
    seed <- as.integer(seed)

    in.list <- list("invert_basic", observed, nspec, modcode,
                    inits, npars, ipars, cons, ncons, icons,
                    pmu, psd, plog, minp, ngibbs, results, seed)

    t1 <- proc.time()
    out.list <- do.call(.Fortran, in.list)
    t2 <- proc.time()
    print(t2 - t1)
    return(out.list[[length(out.list)-1]])
}

