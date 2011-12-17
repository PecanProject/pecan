##' Spline estimate of univariate relationship between parameter value and model output
##'
##' Creates a spline function using the splinefun function that estimates univariate response of parameter input to model output
##'
##' @title Sensitivity spline function 
##' @param quantiles.input 
##' @param quantiles.output 
##' @return function   
sa.splinefun <- function(quantiles.input, quantiles.output){
  return(splinefun(quantiles.input, quantiles.output, method = "monoH.FC"))
}

##' Calculates the standard deviation of the variance estimate
##'
##' Uses the equation \sigma^4\left(\frac{2}{n-1}+\frac{\kappa}{n}\right)
##' @title Standard deviation of sample variance
##' @param x sample
##' @return estimate of standard deviation of the sample variance
##' @references \href{Wikipedia}{http://en.wikipedia.org/wiki/Variance#Distribution_of_the_sample_variance}
sd.var <- function(x){
  var(x, na.rm = TRUE)^2*(2/(sum(!is.na(x))-1) + kurtosis(x)/sum(!is.na(x)))
}

##' Calculates the kurtosis of a vector
##'
##' @title Calculate kurtosis from a vector
##' @param x vector of values
##' @return numeric value of kurtosis
##' @references  NIST/SEMATECH e-Handbook of Statistical Methods, \url{http://www.itl.nist.gov/div898/handbook/eda/section3/eda35b.htm}, 2011-06-20.
kurtosis <- function(x) {
  kappa <- sum((x - mean(x, na.rm = TRUE))^4)/((sum(!is.na(x)) - 1) * sd(x, na.rm = TRUE)^4) - 3
  return(kappa)
}
##' Calculate the sensitivity of a function at the median
##'
##' This function evaluates the sensitivity of a model to a parameter.
##' This is done by evaluating the first derivative of the univariate spline estimate of the model response
##' at the parameter median.
##' @title Calculate Sensitivity
##' @param trait.samples 
##' @param sa.spline 
##' @return numeric estimate of model sensitivity to parameter
get.sensitivity <- function(trait.samples, sa.spline){
  sensitivity <- sa.spline(median(trait.samples, na.rm = TRUE), 1)
}

##' Given a set of numbers (a numeric vector), this returns the set's coefficient of variance.
##'
##' @title Get coefficient of variance 
##' @param set numeric vector of trait values
##' @return coeficient of variance
get.coef.var <- function(set){
  sqrt(var(set)) / mean(set)
}

##' Generic function for the elasticity
##'
##' Given the sensitivity, samples, and outputs for a single trait, return elasticity
##' @title Get Elasticity 
##' @param sensitivity univariate sensitivity of model to a parameter, can be calculated by \link{get.sensitivity}  
##' @param samples samples from trait distribution
##' @param outputs model output from ensemble runs
##' @return elasticity = normalized sensitivity 
get.elasticity <- function(sensitivity, samples, outputs){
  return(sensitivity / (mean(outputs) / mean(samples)))
}
##' Truncates vector at 0
##'
##' @title Zero Truncate 
##' @param y numeric vector
##' @return numeric vector with all values less than 0 set to 0
zero.truncate <- function(y) {
  y[y<0 | is.na(y)] <- 0
  return(y)
}
##' Performs univariate sensitivity analysis and variance decomposition 
##'
##' This function estimates the univariate responses of a model to a parameter for a set of traits, calculates the model sensitivity at the median, and performs a variance decomposition. This function results in a set of sensitivity plots (one per variable) and variance decomposition plot.
##' @title Sensitivity Analysis 
##' @param trait.samples list of vectors, one per trait, representing samples of the trait value, with length equal to the mcmc chain length. Samples are taken from either the prior distribution or meta-analysis results
##' @param sa.samples data.frame with one column per trait and one row for the set of quantiles used in sensitivity analysis. Each cell contains the value of the trait at the given quantile.
##' @param sa.output  list of data.frames, similar to sa.samples, except cells contain the results of a model run with that trait x quantile combination and all other traits held at their median value  
##' @param outdir directory to which plots are written
##' @return results of sensitivity analysis
##' @examples
##' sensitivity.analysis(trait.samples[[pft$name]], sa.samples[[pft$name]], sa.agb[[pft$name]], pft$outdir)
sensitivity.analysis <- function(trait.samples, sa.samples, sa.output, outdir){
  traits <- names(trait.samples)
  sa.splines <- sapply(traits, function(trait) sa.splinefun(sa.samples[[trait]],
                                                               sa.output[[trait]]))
  
  spline.estimates <- lapply(traits, function(trait)
                             zero.truncate(sa.splines[[trait]](trait.samples[[trait]])))
  names(spline.estimates) <- traits
  sensitivities <- sapply(traits, function(trait)
                          get.sensitivity(trait.samples[[trait]],
                                          sa.splines[[trait]]))
  elasticities <- sapply(traits, 
                         function(trait)
                         get.elasticity(sensitivities[[trait]],
                                            trait.samples[[trait]],
                                            spline.estimates[[trait]]))
  variances <- sapply(traits, function(trait)
                      var(spline.estimates[[trait]]))
  partial.variances <- variances / sum(variances)
  
  ## change Vm_low_temp to Kelvin prior to calculation of coefficient of variance.
  ## this conversion is only required at this point in the code, for calculating CV
  C.units <- grep('Celsius', trait.dictionary(traits)$units, ignore.case = TRUE)
  trait.samples[[C.units]] <- trait.samples[[C.units]] + 273.15

  coef.vars <- sapply(trait.samples, get.coef.var)
  outlist <- list(sensitivity.plot.inputs = list(
                    sa.samples    = sa.samples,
                    sa.splines = sa.splines),
                  variance.decomposition.plot.inputs = list(
                    coef.vars         = coef.vars,
                    elasticities      = elasticities,
                    sensitivities     = sensitivities,
 #                   variances         = variances,
                    partial.variances = partial.variances))
  return(outlist)
}

