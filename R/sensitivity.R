##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param quantiles.input 
##' @param quantiles.output 
##' @return spline function from splinefun that estimates univariate response of parameter input to model output  
##' @author David
sa.splinefun <- function(quantiles.input, quantiles.output){
  return(splinefun(quantiles.input, quantiles.output, method = "monoH.FC"))
}

sd.var <- function(x){
  var(x)^2*(2/(length(x)-1) + kurtosis(x)/length(x))
}
get.sensitivity <- function(trait.samples, sa.splinefun){
  sensitivity <- sa.splinefun(mean(trait.samples), 1)
}
#Given a set of numbers, this returns the set's coefficient of variance
get.coef.var <- function(set){
  sqrt(var(set)) / mean(set)
}
#Given the sensitivity, samples, and outputs for a single trait, return elasticity
get.elasticity <- function(sensitivity, samples, outputs){
  return(sensitivity / (mean(outputs) / mean(samples)))
}

zero.truncate <- function(y) {
  y[y<0 | is.na(y)] <- 0
  return(y)
}


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
