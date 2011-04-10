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
#Given a set of numerical sets, 
#this returns the fraction each set contributes to the total variance
get.explained.variances <- function(sets){
  set.variances <- apply(sets, 2, var)
  total.variance <- sum(set.variances)
  return(set.variances / total.variance)
}

zero.truncate <- function(y) {
  y[y<0 | is.na(y)] <- 0
  return(y)
}

sensitivity.analysis <- function(trait.samples, sa.samples, sa.output){
  traits <- names(trait.samples)
  sa.splinefuns <- sapply(traits, function(trait) sa.splinefun(sa.samples[[trait]], sa.output[[trait]]))
  
  saplots <-  lapply(traits, function(x) sensitivity.plot(sa.samples[[x]], sa.splinefuns[[x]], x))
  pdf('sensitivity.analysis.pdf', height = 12, width = 20)
  saplots#left='Aboveground Biomass', main='Parameter Sensitivity', nrow=3,ncol=5) 
  dev.off()
  
  spline.estimates <- sapply(traits, function(trait) sa.splinefuns[[trait]](trait.samples[[trait]]))
  spline.estimates <- zero.truncate(spline.estimates)
  sensitivities <- sapply(traits, function(trait) get.sensitivity(trait.samples[[trait]], sa.splinefuns[[trait]]))
  elasticities <- sapply(traits, 
      function(trait) get.elasticity(sensitivities[[trait]], trait.samples[[trait]], spline.estimates[,trait]))
  explained.variances <- get.explained.variances(spline.estimates)
  
  #TODO: move unit conversions to their own method, called before sensitivity analysis 
  if('Vm_low_temp' %in% traits)
    trait.samples[which(traits == 'Vm_low_temp')] <- trait.samples[which(traits == 'Vm_low_temp')] + 273.15
  coef.vars <- sapply(trait.samples, get.coef.var)
  
  plot.variance.decomposition(coef.vars, elasticities, explained.variances)
}
