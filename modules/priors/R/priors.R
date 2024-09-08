##' Fit a distribution to data
##'
##' @title Fit distribution to data  
##' @param trait.data data for distribution
##' @param trait name of trait to fit.
##'  One of "tt", "sla", "rrr", "q"
##' @param dists list of distribution names
##' @param n only used in return value
##' @return best fit distribution
##' @export
##' @author David LeBauer
fit.dist <- function(trait.data, trait = colnames(trait.data), 
                     dists = c("weibull", "lognormal", "gamma"), n = NULL) {
  
  if (inherits(trait.data, "data.frame")) {
    trait.data <- trait.data[, 1]
  }
  ## warning(immediate. = TRUE)
  nostart.dists <- dists[dists %in% c("weibull", "lognormal", "gamma", "normal")]
  a <- lapply(nostart.dists, function(x) suppressWarnings(MASS::fitdistr(trait.data, x)))
  names(a) <- nostart.dists
  if ("f" %in% dists) {
    print(trait)
    if (trait == "tt") {
      a[["f"]] <- suppressWarnings(MASS::fitdistr(trait.data, "f", 
                                            start = list(df1 = 100, df2 = 200)))
    } else if (trait == "sla") {
      a[["f"]] <- suppressWarnings(MASS::fitdistr(trait.data, "f", 
                                            start = list(df1 = 6, df2 = 1)))
    } else if (trait == "rrr") {
      a[["f"]] <- suppressWarnings(MASS::fitdistr(trait.data, "f", 
                                            start = list(df1 = 6, df2 = 1)))
    } else if (trait == "q") {
      a[["f"]] <- suppressWarnings(MASS::fitdistr(trait.data, "f", 
                                            start = list(df1 = 1, df2 = 2)))
    } else {
      PEcAn.logger::logger.severe(paste(trait, "not supported!"))
    }
  }
  if ("beta" %in% dists) {
    a[["beta"]] <- suppressWarnings(MASS::fitdistr(trait.data, "beta", 
                                             start = list(shape1 = 2, shape2 = 1)))
  }
  aicvalues <- lapply(a, stats::AIC)
  result <- t(sapply(
    dists,
    function(x) cbind(
      t(PEcAn.utils::tabnum(a[[x]]$estimate)),
      signif(aicvalues[[x]]))
  ))
  colnames(result) <- c("a", "b", "AIC")
  print(result)
  bestfitdist <- names(which.min(aicvalues))
  parms <- PEcAn.utils::tabnum(a[[bestfitdist]]$estimate)
  dist  <- ifelse(bestfitdist == "normal", "norm", bestfitdist)
  return(data.frame(distribution = dist,
                    a = as.numeric(parms[1]),
                    b = as.numeric(parms[2]), 
                    n = ifelse(is.null(n), length(trait.data), n)))
} # fit.dist


#--------------------------------------------------------------------------------------------------#
##' Prior fitting function for optimization
##'
##' This function is used within `DEoptim` to parameterize a distribution to the 
##' central tendency and confidence interval of a parameter. 
##' This function is not very robust; currently it needs to be tweaked when distributions
##' require starting values (e.g. beta, f)
##' @title prior.fn 
##' @param parms target for optimization
##' @param x vector with c(lcl, ucl, ct) lcl / ucl = confidence limits, ct = entral tendency 
##' @param alpha quantile at which lcl/ucl are estimated (e.g. for a 95\% CI, alpha = 0.5)
##' @param distn named distribution, one of 'lnorm', 'gamma', 'weibull', 'beta'; support for other distributions not currently implemented 
##' @param central.tendency one of 'mode', 'median', and 'mean' 
##' @param trait name of trait, can be used for exceptions (currently used for trait == 'q')
##' @export
##' @return parms
##' @author David LeBauer
##' @examples
##' \dontrun{
##'   DEoptim(fn = prior.fn,
##'           lower = c(0, 0),
##'           upper = c(1000, 1000),
##'           x=c(2, 6, 3.3),
##'           alpha = 0.05,
##'           distn = 'lnorm')$optim$bestmem
##' }
##'
prior.fn <- function(parms, x, alpha, distn, central.tendency = NULL, trait = NULL) {
  if (!distn %in% c("lnorm", "gamma", "weibull", "beta")) {
    stop(paste(distn, "not currently supported by prior.fn"))
  }
  if (distn == "lnorm") {
    mu <- parms[1]
    sigma <- parms[2]
    lcl <- mu + stats::qnorm(alpha / 2) * sigma
    ucl <- mu + stats::qnorm(1 - alpha / 2) * sigma
    if (is.null(central.tendency)) {
      ct <- x[3]
    } else if (central.tendency == "mean") {
      ct <- mu - sigma^2
    } else if (central.tendency == "median") {
      ct <- stats::qlnorm(0.5, parms[1], parms[2])
    } else {
      PEcAn.logger::logger.severe(paste(central.tendency, "not supported!"))
    }
    x <- log(x)
  }
  if (distn == "gamma") {
    lcl <- stats::qgamma(alpha / 2, parms[1], parms[2])
    ucl <- stats::qgamma(1 - alpha / 2, parms[1], parms[2])
    if (is.null(central.tendency)) {
      ct <- x[3]
    } else if (central.tendency == "median") {
      ct <- stats::qgamma(0.5, parms[1], parms[2])
    } else if (central.tendency == "mean") {
      ct <- parms[1] / parms[2]
    } else if (central.tendency == "mode") {
      ct <- ifelse(parms[1] > 1, (parms[1] - 1) / parms[2], 0)
    } else {
      stop("Unknown central.tendency!")
    }
  }
  if (distn == "weibull") {
    lcl <- stats::qweibull(alpha / 2, parms[1], parms[2])
    ucl <- stats::qweibull(1 - alpha / 2, parms[1], parms[2])
    if (is.null(central.tendency)) {
      ct <- x[3]
    } else if (central.tendency == "median") {
      ct <- parms[2] * log(2) ^ (1 / parms[1])
    } else if (central.tendency == "mean") {
      ct <- parms[2] * gamma(1 + 1 / parms[2])
    } else if (central.tendency == "mode") {
      stop("mode calculation not currently supported for weibull distribution")
    } else {
      stop("Unknown central.tendency!")
    }
  }
  if (distn == "beta") {
    a <- parms[1]
    if (central.tendency == "mean" & trait == "fineroot2leaf") {
      ## fixed mean, optimize for a
      b <- a * (1 / x[3] - 1)
    } else {
      b <- parms[2]
    }
    lcl <- stats::qbeta(alpha / 2, a, b)
    ucl <- stats::qbeta(1 - alpha / 2, a, b)
    if (is.null(central.tendency)) {
      ct <- x[3]
    } else if (central.tendency == "mean") {
      ct <- a / (a + b)
    } else if (central.tendency == "median") {
      ct <- stats::qbeta(0.5, a, b)  ## median
    } else if (central.tendency == "mode") {
      ct <- ifelse(a > 1 & b > 1, (a - 1) / (a + b - 2), 0)  ## mode
    }
  }
  return(sum(abs(c(lcl, ucl, ct) - x)))
} # prior.fn


#--------------------------------------------------------------------------------------------------#
##' Take n random samples from prior
##'
##' @title Sample from prior 
##' @param distn name of distribution, e.g. "norm", "pois"
##' @param parama first parameter for distn call
##' @param paramb second parameter for distn call
##' @param n number of samples to return
##' @return vector with n random samples from prior
##' @export
##' @seealso \code{\link{get.sample}}
pr.samp <- function(distn, parama, paramb, n) {
  return(do.call(paste0("r", distn), list(n, parama, paramb)))
} # pr.samp


#--------------------------------------------------------------------------------------------------#
##' Take n random samples from prior
##'
##' Similar to the prior sample function \link{pr.samp}, except 1) it takes the prior as a named dataframe
##' or list and it can return either a random sample of length n OR a sample from a quantile specified as p
##' @title Get Samples
##' @param prior data.frame with distn, parama, and optionally paramb.
##' @param n number of samples to return from a random sample of the rdistn family of functions (e.g. rnorm)
##' @param p vector of quantiles from which to sample the distribution; typically pre-generated upstream
##' in the workflow to be used by the qdistn family of functions (e.g. qnorm)
##' @return vector with n random samples from prior
##' @seealso \link{pr.samp}
##' @examples
##' \dontrun{
##' # return 1st through 99th quantile of standard normal distribution:
##' PEcAn.priors::get.sample(
##'    prior = data.frame(distn = 'norm', parama = 0, paramb = 1), 
##'    p = 1:99/100)
##' # return 100 random samples from standard normal distribution:
##' PEcAn.priors::get.sample(
##'    prior = data.frame(distn = 'norm', parama = 0, paramb = 1), 
##'    n = 100)
##' }
##' @export
get.sample <- function(prior, n = NULL, p = NULL) {
  if(!is.null(p)){
    if (as.character(prior$distn) %in% c("exp", "pois", "geom")) {
      ## one parameter distributions
      return(do.call(paste0("q", prior$distn), list(p, prior$parama)))
    } else {
      ## two parameter distributions
      return(do.call(paste0("q", prior$distn), list(p, prior$parama, prior$paramb)))
    }
  }
  if (as.character(prior$distn) %in% c("exp", "pois", "geom")) {
    ## one parameter distributions
    return(do.call(paste0("r", prior$distn), list(n, prior$parama)))
  } else {
    ## two parameter distributions
    return(do.call(paste0("r", prior$distn), list(n, prior$parama, prior$paramb)))
  }
} # get.sample


#--------------------------------------------------------------------------------------------------#
##' Calculates density at n points across the range of a parameter
##'
##' For a distribution and parameters, return the density for values ranging from alpha to 1-alpha 
##' @title Calculate densities
##' @param distn distribution
##' @param parama parameter
##' @param paramb parameter
##' @param n length of vector to be returned
##' @param alpha sets range at which the distribution will be evaluated (e.g. from alpha to 1-alpha)
##' @return dataframe with equally spaced x values and the corresponding densities
##' @export
##' @author David LeBauer
pr.dens <- function(distn, parama, paramb, n = 1000, alpha = 0.001) {
  alpha <- ifelse(alpha < 0.5, alpha, 1 - alpha)
  n <- ifelse(alpha == 0.5, 1, n)
  range.x <- do.call(paste("q", distn, sep = ""), 
                     list(c(alpha, 1 - alpha), parama, paramb))
  seq.x <- seq(from = range.x[1], to = range.x[2], length.out = n)
  dens.df <- data.frame(x = seq.x, 
                        y = do.call(paste("d", distn, sep = ""), 
                                    list(seq.x, parama, paramb)))
  return(dens.df)
} # pr.dens


##--------------------------------------------------------------------------------------------------#
##' Create Density Data Frame from Sample
##'
##' Returns a data frame from `stats::density` function 
##'
##' @param samps a vector of samples from a distribution
##' @param zero.bounded logical: Restrict density distribution to nonnegative values?
##' @param distribution list with elements distn, parama, paramb,
##' e.g. \code{list('norm', 0, 1)}
##' @param n number of points at which to estimate density
##' @param ... additional arguments, passed on to `stats::density`
##'
##' @md
##' @author David LeBauer
##' @export
##' @return data frame with x and y = dens(x)
##' @examples
##' prior.df <- create.density.df(distribution = list('norm',0,1))
##' plot(prior.df)
##' samp.df <- create.density.df(samps = rnorm(100))
##' lines(samp.df)
create.density.df <- function(samps = NULL, zero.bounded = FALSE, distribution = NULL, 
                              n = 1000, ...) {
  samp.exists <- !is.null(samps)
  dist.exists <- !is.null(distribution)
  if (identical(samp.exists, dist.exists)) {
    stop("create.density.df requires one and only one of:
         samps: a vector of samples, e.g. MCMC chain,
         OR
         distribution: a named distribution supported by R")
  }
  if (samp.exists) {
    if (zero.bounded) {
      new.density <- PEcAn.utils::zero.bounded.density(samps, n = 1000, ...)
    } else {
      new.density <- stats::density(samps, n = 1000, ...)
    }
    density.df <- with(new.density, data.frame(x = x, y = y))
  }
  
  if (dist.exists) {
    density.df <- do.call(pr.dens, c(distribution[1:3]))
  }
  return(density.df)
} # create.density.df
