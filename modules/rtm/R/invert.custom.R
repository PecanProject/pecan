#' @name invert.custom
#'
#' @title Bayesian inversion of a model
#' @details Performs an inversion of an arbitrary model using a modified 
#' Metropolis Hastings algorithm with block sampling. This may be slightly 
#' slower than the implementation in Fortran, but is much more customizable, as 
#' the model can be any R function.
#' @param observed Vector, matrix, or data frame (coerced to matrix) of 
#' observed values. For spectral data, wavelengths are rows and spectra are 
#' columns. Dimensions must align with the output of `model`.
#' @param invert.options R list object containing the following elements:
#' 
#' inits Vector of initial values of model parameters to be inverted.
#'
#' ngibbs Number of MCMC iterations
#'
#' prior.function Function for use as prior.
#' Should take a vector of parameters as input and return a single value -- the sum of their log-densities -- as output.
#'
#' param.mins Vector of minimum values for inversion parameters
#' 
#' param.maxs Vector of minimum values for inversion parameters
#'
#' model The model to be inverted.
#' This should be an R function that takes `params` and `seed` as input and returns one column of `observed` (nrows should be the same).
#' Constants should be implicitly included here.
#'
#' adapt Number of steps for adapting covariance matrix (i.e. adapt every 'n' steps).
#' Default=100
#' 
#' adj_min Minimum threshold for rescaling Jump standard deviation.
#' Default = 0.1.
#' 
#' target Target acceptance rate.
#' Default=0.234, based on recommendation for multivariate block sampling in Haario et al. 2001
#' 
#' do.lsq Perform least squares optimization first (see `invert.lsq`), and use outputs to initialize Metropolis Hastings.
#' This may improve mixing time, but risks getting caught in a local minimum.
#' Default=FALSE
#' 
#' @param quiet Suppress progress bar and status messages. Default=FALSE
#' @param return.resume If TRUE, return results as list that includes current Jump distribution (useful for continuing an ongoing run) and acceptance rate. Default = FALSE.
#' @param seed Run-unique ID. Useful for parallel runs. Default=NULL
#' @export
invert.custom <- function(observed, invert.options, quiet = FALSE, return.resume = FALSE, seed = NULL) {
  testForPackage("MASS")
  observed <- as.matrix(observed)
  nspec <- ncol(observed)
  nwl <- nrow(observed)

  need.invert.options <- c("inits", "ngibbs", "prior.function", "param.mins", "param.maxs",
                           "adapt", "adj_min", "target", "do.lsq", "model")
  have.invert.options <- names(invert.options)
  overlap.invert.options <- need.invert.options %in% have.invert.options
  if (any(!overlap.invert.options)) {
    error.msg <- paste("Missing the following invert.options:",
                       paste(need.invert.options[!overlap.invert.options], collapse=" "),
                       "Try modifying a default.invert.options() object",
                       sep = "\n")
    stop(error.msg)
  }
  
  # Unpack invert.options list
  model <- invert.options$model
  inits <- invert.options$inits
  ngibbs <- invert.options$ngibbs
  prior.function <- invert.options$prior.function
  param.mins <- invert.options$param.mins
  if (length(inits) != length(param.mins)) {
    stop(sprintf("Length mismatch between inits (%d) and param.mins (%d)", length(inits), length(param.mins)))
  }
  param.maxs <- invert.options$param.maxs
  if (length(inits) != length(param.maxs)) {
    stop(sprintf("Length mismatch between inits (%d) and param.maxs (%d)", length(inits), length(param.maxs)))
  }
  adapt <- invert.options$adapt
  adj_min <- invert.options$adj_min
  target <- invert.options$target
  do.lsq <- invert.options$do.lsq
  resume <- invert.options$resume
  init.Jump <- resume$jump
  init.ar <- resume$ar
  
  # If `model` doesn't have a seed argument (second argument), add it.
  model.args <- names(formals(model))
  if (length(model.args) != 2) {
    warning("Model was missing 'seed' argument. Adding as empty argument.")
    model <- function(params, seed = NULL) invert.options$model(params)
  }
  
  # Set constants for inversion
  tau_0 <- 0.001
  init_rsd <- 0.5
  init_jump_diag_factor <- 0.05
  # Set up inversion
  npars <- length(inits)
  if (do.lsq) {
    fit <- invert.lsq(observed, inits, model, 
                      lower = param.mins, upper = param.maxs)
    inits <- fit$par
  }
  rp1 <- tau_0 + nspec * nwl/2
  rsd <- 0.5
  PrevSpec <- model(inits, seed)
  PrevError <- PrevSpec - observed
  if (is.null(init.Jump)) {
    # Set initial standard deviation to small fraction of initial
    # values (absolute value because SD can't be negative)
    initsd <- abs(inits) * init_jump_diag_factor
    initsd[initsd == 0] <- 1e-5
    Jump <- diag(initsd)
  } else {
    Jump <- init.Jump
  }
  if (!all(diag(Jump) > 0)) {
    stop("Negative or zero values in diagonal of Jump covariance matrix")
  }
  results <- matrix(NA, nrow = ngibbs, ncol = npars + 1)
  if (!is.null(names(inits))) {
    cnames <- names(inits)
  } else {
    cnames <- sprintf("par%d", seq_along(inits))
  }
  colnames(results) <- c(cnames, "residual")
  if (is.null(init.ar)) {
    ar <- 0
  } else {
    ar <- init.ar
  }
  if (!quiet) {
    pb <- txtProgressBar(min = 0, max = ngibbs, style = 3)
  }
  for (ng in seq_len(ngibbs)) {
    if (!quiet) { 
      setTxtProgressBar(pb, ng)
    }
    if (ng %% adapt < 1) {
      if (ar < 2) {
        rescale <- diag(rep(adj_min, npars))
        Jump <- rescale %*% Jump %*% rescale
      } else {
        adj <- max(ar / adapt / target, adj_min)
        region <- seq(ng - adapt, ng - 1)
        stdev <- apply(results[region, 1:npars], 2, sd)
        rescale <- diag(stdev * adj)
        cormat <- cor(results[region, 1:npars])
        if (any(is.na(cormat))) {
          cormat <- diag(rep(1, npars))
        }
        Jump <- rescale %*% cormat %*% rescale
      }
      ar <- 0
    }
    tvec <- MASS::mvrnorm(1, inits, Jump)
    if (all(tvec > param.mins & tvec < param.maxs)) {
      TrySpec <- model(tvec, seed)
      TryError <- TrySpec - observed
      TryPost <- sum(dnorm(TryError, 0, rsd, 1)) + prior.function(tvec)
      PrevPost <- sum(dnorm(PrevError, 0, rsd, 1)) + prior.function(inits)
      a <- exp(TryPost - PrevPost)
      if (is.na(a)) {
        a <- -1
      }
      if (a > runif(1)) {
        inits <- tvec
        PrevError <- TryError
        ar <- ar + 1
      }
    }
    results[ng, 1:npars] <- inits
    rp2 <- tau_0 + sum(PrevError * PrevError)/2
    rinv <- rgamma(1, rp1, rp2)
    rsd <- 1/sqrt(rinv)
    results[ng, npars + 1] <- rsd
  }
  if (!quiet) {
    close(pb)
  }
  if (return.resume) {
    out <- list(results = results, resume = list(jump = Jump, ar = ar))
    return(out)
  } else {
    return(results)
  }
}

