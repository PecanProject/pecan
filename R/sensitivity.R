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
##' Uses the equation 
##' @title 
##' @param x 
##' @return 
##' @author David
sd.var <- function(x){
  var(x)^2*(2/(length(x)-1) + kurtosis(x)/length(x))
}

get.sensitivity <- function(trait.samples, sa.splinefun){
  sensitivity <- sa.splinefun(mean(trait.samples), 1)
}

                          
##' Given a set of numbers, this returns the set's coefficient of variance.
##'
##' @title Get coefficient of variance 
##' @param set 
##' @return 
get.coef.var <- function(set){
  sqrt(var(set)) / mean(set)
}

##' Given the sensitivity, samples, and outputs for a single trait, return elasticity
##' 
##' @title Get elasticity 
##' @param sensitivity 
##' @param samples 
##' @param outputs 
##' @return numeric value, elasticity = normalized sensitivity 
get.elasticity <- function(sensitivity, samples, outputs){
  return(sensitivity / (mean(outputs) / mean(samples)))
}
##' Truncates vector at 0
##'
##' @title Zero truncate 
##' @param y numeric vector
##' @return numeric vector with all values less than 0 set to 0
zero.truncate <- function(y) {
  y[y<0 | is.na(y)] <- 0
  return(y)
}
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param trait.samples list of vectors, one per trait, representing samples of the trait value, with length equal to the mcmc chain length. Samples are taken from either the prior distribution or meta-analysis results
##' @param sa.samples list of data.frames, one per pft. each data frame contains one column per trait and one row for the set of quantiles used in sensitivity analysis. Each cell contains the value of the trait at the given quantile
##' @param sa.output  list of data.frames, similar to sa.samples, except cells contain the results of a model run with that trait x quantile combination and all other traits held at their median value  
##' @param outdir directory to which plots are written
##' @return 
##' @examples
##' sensitivity.analysis(trait.samples[[pft$name]], sa.samples[[pft$name]], sa.agb[[pft$name]], pft$outdir)
sensitivity.analysis <- function(trait.samples, sa.samples, sa.output, outdir){
  traits <- names(trait.samples)
  sa.splinefuns <- sapply(traits, function(trait) sa.splinefun(sa.samples[[trait]], sa.output[[trait]]))
  
  spline.estimates <- lapply(traits, function(trait) zero.truncate(sa.splinefuns[[trait]](trait.samples[[trait]])))
  names(spline.estimates) <- traits
  sensitivities <- sapply(traits, function(trait) get.sensitivity(trait.samples[[trait]], sa.splinefuns[[trait]]))
  elasticities <- sapply(traits, 
      function(trait) abs(get.elasticity(sensitivities[[trait]], trait.samples[[trait]], spline.estimates[[trait]])))
  variances <- sapply(traits, function(trait) var(spline.estimates[[trait]]))
  explained.variances <- variances / sum(variances)
  
  #TODO: move unit conversions to their own method, called before sensitivity analysis 
  if('Vm_low_temp' %in% traits)
    trait.samples[[which(traits == 'Vm_low_temp')]] <- trait.samples[[which(traits == 'Vm_low_temp')]] + 273.15
  coef.vars <- sapply(trait.samples, get.coef.var)
  plot.sensitivities(sa.samples, sa.splinefuns, outdir)  
  plot.variance.decomposition(coef.vars, elasticities, explained.variances, outdir)
}

