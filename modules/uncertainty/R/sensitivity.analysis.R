#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------#
##' Spline estimate of univariate relationship between parameter value and model output
##'
##' Creates a spline function using the splinefun function that estimates univariate response of parameter input to model output
##' @name sa.splinefun
##' @title Sensitivity spline function 
##' @param quantiles.input 
##' @param quantiles.output 
##' @export
##' @return function   
sa.splinefun <- function(quantiles.input, quantiles.output){
  return(splinefun(quantiles.input, quantiles.output, method = "monoH.FC"))
}
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##' Calculates the standard deviation of the variance estimate
##'
##' Uses the equation \sigma^4\left(\frac{2}{n-1}+\frac{\kappa}{n}\right)
##' @name sd.var
##' @title Standard deviation of sample variance
##' @param x sample
##' @return estimate of standard deviation of the sample variance
##' @export
##' @author David LeBauer
##' @references  Mood, Graybill, Boes 1974 "Introduction to the Theory of Statistics" 3rd ed. p 229; Casella and Berger "Statistical Inference" p 364 ex. 7.45; "Reference for Var(s^2)" CrossValidated \url{http://stats.stackexchange.com/q/29905/1381}, "Calculating required sample size, precision of variance estimate" CrossValidated \url{http://stats.stackexchange.com/q/7004/1381}, "Variance of Sample Variance?" Mathematics - Stack Exchange \url{http://math.stackexchange.com/q/72975/3733}
sd.var <- function(x){
  var(x, na.rm = TRUE)^2*(2/(sum(!is.na(x))-1) + kurtosis(x)/sum(!is.na(x)))
}
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##' Calculates the excess kurtosis of a vector
##'
##' Note that this calculates the "excess kurtosis", which is defined as kurtosis - 3.
##' This statistic is used in the calculation of the standard deviation of sample variance
##' in the function \code{\link{sd.var}}.  
##' Additional details 
##' @name kurtosis
##' @title Calculate excess kurtosis from a vector
##' @param x vector of values
##' @return numeric value of kurtosis
##' @author David LeBauer
##' @references  NIST/SEMATECH e-Handbook of Statistical Methods, \url{http://www.itl.nist.gov/div898/handbook/eda/section3/eda35b.htm}, 2011-06-20.
kurtosis <- function(x) {
  kappa <- sum((x - mean(x, na.rm = TRUE))^4)/((sum(!is.na(x)) - 1) * sd(x, na.rm = TRUE)^4) - 3
  return(kappa)
}
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##' Calculate the sensitivity of a function at the median
##'
##' This function evaluates the sensitivity of a model to a parameter.
##' This is done by evaluating the first derivative of the univariate spline estimate
##' of the model response at the parameter median.
##' @name get.sensitivity
##' @title Calculate Sensitivity
##' @param trait.samples 
##' @param sa.splinefun 
##' @export
##' @return numeric estimate of model sensitivity to parameter
get.sensitivity <- function(trait.samples, sa.splinefun){
  sensitivity <- sa.splinefun(median(trait.samples), 1)
}
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##' Given a set of numbers (a numeric vector), this returns the set's coefficient of variance.
##'
##' @name get.coef.var
##' @title Get coefficient of variance 
##' @param set numeric vector of trait values
##' @export
##' @return coeficient of variance
get.coef.var <- function(set){
  sqrt(var(set)) / median(set)
}
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##' Generic function for the elasticity
##'
##' Given the sensitivity, samples, and outputs for a single trait, return elasticity
##' @name get.elasticity
##' @title Get Elasticity 
##' @param sensitivity univariate sensitivity of model to a parameter, can be calculated by \code{\link{get.sensitivity}}
##' @param samples samples from trait distribution
##' @param outputs model output from ensemble runs
##' @export
##' @return elasticity = normalized sensitivity 
get.elasticity <- function(sensitivity, samples, outputs){
  return(sensitivity / (median(outputs) / median(samples)))
}
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##' Performs univariate sensitivity analysis and variance decomposition 
##'
##' This function estimates the univariate responses of a model to a parameter for a set of traits, calculates the model sensitivity at the median, and performs a variance decomposition. This function results in a set of sensitivity plots (one per variable) and variance decomposition plot.
##' @name sensitivity.analysis
##' @title Sensitivity Analysis 
##' @param trait.samples list of vectors, one per trait, representing samples of the trait value, with length equal to the mcmc chain length. Samples are taken from either the prior distribution or meta-analysis results
##' @param sa.samples data.frame with one column per trait and one row for the set of quantiles used in sensitivity analysis. Each cell contains the value of the trait at the given quantile.
##' @param sa.output  list of data.frames, similar to sa.samples, except cells contain the results of a model run with that trait x quantile combination and all other traits held at their median value  
##' @param outdir directory to which plots are written
##' @return results of sensitivity analysis
##' @author David LeBauer
##' @examples
##' \dontrun{
##' sensitivity.analysis(trait.samples[[pft$name]], sa.samples[[pft$name]], sa.agb[[pft$name]], pft$outdir)
##' }
sensitivity.analysis <- function(trait.samples, sa.samples, sa.output, outdir){
  traits <- names(trait.samples)
  sa.splines <- sapply(traits, function(trait) sa.splinefun(sa.samples[[trait]],
            sa.output[[trait]]))
  
  spline.estimates <- lapply(traits, function(trait)
        spline.truncate(sa.splines[[trait]](trait.samples[[trait]])))
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
  partial.variances <- variances #/ sum(variances)

  coef.vars <- sapply(trait.samples, get.coef.var)
  outlist <- list(sensitivity.output = list(
                    sa.samples    = sa.samples,
                    sa.splines = sa.splines),
                  variance.decomposition.output = list(
                    coef.vars         = coef.vars,
                    elasticities      = elasticities,
                    sensitivities     = sensitivities,
                    variances         = variances,
                    partial.variances = partial.variances))
  return(outlist)
}

##' Truncate spline at zero if..
##'
##' Truncate spline at zero if P[x<0] < pnorm(-3)
##' pnorm(-3) chosen as default value for min quantile
##' because this is the default low end of range for the
##' sensitivity analysis.
##' This parameter could be determined based on minimum value in
##' settings$sensitivity.analysis$quantiles
##' @title Truncate spline 
##' @param x vector
##' @param min.quantile threshold quantile for testing lower bound on variable
##' @return either x or a vector with values < 0 converted to zero
##' @author David LeBauer
##' @export
##' @examples
##' set.seed(0)
##' x <- c(rgamma(998,1,1), rnorm(10)) 
##' min(x) # -0.5238
##' min(PEcAn.uncertainty::spline.truncate(x))
spline.truncate <- function(x, min.quantile = pnorm(-3)){
  if(quantile(x, min.quantile) > 0){
     x <- zero.truncate(x)
   }
  return(x)
}

####################################################################################################
### EOF.  End of R script file.              
####################################################################################################
