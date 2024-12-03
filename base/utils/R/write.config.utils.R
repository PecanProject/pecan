
#--------------------------------------------------------------------------------------------------#
### TODO: Generalize this code for all ecosystem models (e.g. ED2.2, SiPNET, etc).
#--------------------------------------------------------------------------------------------------#

#' Get Quantiles
#'
#' Returns a vector of quantiles specified by a given `<quantiles>` xml tag
#'
#' @param quantiles.tag specifies tag used to specify quantiles
#' @return vector of quantiles
#' @export
#' @author David LeBauer
get.quantiles <- function(quantiles.tag) {
  quantiles <- vector()
  if (!is.null(quantiles.tag$quantile)) {
    quantiles <- as.numeric(quantiles.tag[names(quantiles.tag) == "quantile"])
  }
  if (!is.null(quantiles.tag$sigma)) {
    sigmas <- as.numeric(quantiles.tag[names(quantiles.tag) == "sigma"])
    quantiles <- append(quantiles, 1 - stats::pnorm(sigmas))
  }
  if (length(quantiles) == 0) {
    quantiles <- 1 - stats::pnorm(-3:3)  #default
  }
  if (!0.5 %in% quantiles) {
    quantiles <- append(quantiles, 0.5)
  }
  return(sort(quantiles))
} # get.quantiles


#' get sensitivity samples as a list
#'
#' @param pft list of samples from Plant Functional Types
#' @param env list of samples from environment parameters
#' @param quantiles quantiles at which to obtain samples from parameter for
#' sensitivity analysis
#' @export
#' @return sa.sample.list
get.sa.sample.list <- function(pft, env, quantiles) {
  sa.sample.list <- list()
  for (i in seq_along(pft)) {
    sa.sample.list[[i]] <- get.sa.samples(pft[[i]], quantiles)
  }
  sa.sample.list[[length(pft) + 1]] <- get.sa.samples(env, quantiles)
  names(sa.sample.list) <- c(names(pft), "env")
  return(sa.sample.list)
} # get.sa.sample.list


#' Get sensitivity analysis samples
#'
#' Samples parameters for a model run at specified quantiles.
#'
#' Samples from long (>2000) vectors that represent random samples from a
#'   trait distribution.
#' Samples are either the MCMC chains output from the Bayesian meta-analysis
#'   or are randomly sampled from the closed-form distribution of the
#'   parameter probability distribution function.
#' The list is indexed first by trait, then by quantile.
#'
#' @param samples random samples from trait distribution
#' @param quantiles list of quantiles to at which to sample,
#'   set in settings file
#' @return a list of lists representing quantile values of trait distributions
#' @export
#' @author David LeBauer
get.sa.samples <- function(samples, quantiles) {
  sa.samples <- data.frame()
  for (trait in names(samples)) {
    for (quantile in quantiles) {
      sa.samples[as.character(round(quantile * 100, 3)), trait] <- 
        quantile(samples[[trait]], quantile)
    }
  }
  return(sa.samples)
} # get.sa.samples


#' checks that met2model function exists
#'
#' Checks if `met2model.<model>` exists for a particular model
#'
#' @param model model package name
#' @return logical
met2model.exists <- function(model) {
  load.modelpkg(model)
  return(exists(paste0("met2model.", model)))
} # met2model.exists
