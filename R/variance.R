##' Variance and SD(variance)
##'
##' calculates variance and sd of variance using the \code{var} from base R and \code{\link{sd.var}} from PEcAn.
##' @title variance statistics
##' @param x numeric vector
##' @return list with variance and sd of variance
##' @author David LeBauer
variance.stats <- function(x){
  list(var = var(x), sd = sd.var(x))
}

##' .. content for \description{} (no empty lines) ..
##'
##' Given a matrix of parameter sets, calculates a matrix of model output by applying appropriate spline function to each parameter. Output is use with \link{\code{variance.decomposition}} and \link{\code{spline.ensemble}}  
##' @title Get g_i(phi_i)
##' @param splinefuns univariate spline functions created for each trait, e.g. by the \link{\code{sensitivity.analysis}} function. 
##' @param trait.samples n x m matrix (or list with m vectors of length n) of n parameter sets, each with a sample from m traits 
##' @param maxn maximum number of parameter sets to evaluate
##' @return matrix of spline estimates of model output for each of n parameter sets 
##' @author David LeBauer
get.gi.phii <- function(splinefuns, trait.samples, maxn = NULL){
  ## check inputs
  if(class(trait.samples) == 'list'){
    trait.samples <- matrix(unlist(trait.samples), 
                            ncol = length(names(trait.samples)))
    colnames(trait.samples) <- names(splinefuns)
    if(!is.null(maxn) & maxn < nrow(trait.samples)){
      j <- sample(1:nrow(trait.samples), maxn)
      trait.samples <- trait.samples[j, ]
    }
  }
  if(class(trait.samples) != 'matrix'){
    stop(paste('variance.decomposition currently does not handle trait.samples of class', class(trait.samples), '\n please convert to list or matrix'))
  }
  if(!all(names(splinefuns) %in% colnames(trait.samples))){
    stop('mismatch between splinefuns and samples')
  }
  traits <- names(splinefuns)
     
  ## g_i(phi_i) the spline estimate of model output for value of trait i
  gi.phii <- t(laply(traits, 
                    function(x) splinefuns[[x]](trait.samples[,x])))
  colnames(gi.phii) <- traits
  return(gi.phii)
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Spline Ensemble
##' @author David LeBauer
##' @param gi.phii matrix given as output from \link{\code{get.gi.phii}}
##' @param median median value around which variance will be calculated
spline.ensemble <- function(gi.phii, median){
  ## Calculate ensemble output for each parameter set (each row of trait.samples)
  ## Equation 3
  ##  1. calculate residuals  (g_i(phi_i,j) - g_i(median))
  residuals <- gi.phii - median
  ## 2. sum residuals by row, then truncate at 0:
  spline.estimate <- sapply(rowSums(residuals),
                            function(x) max(0, x + median))
  return(spline.estimate)
}

vd.variance <- function(gi.phii){
  ## Calculate variance for each trait
  var.phii    <- apply(gi.phii, 2, var)
  sd.var.phii <- apply(gi.phii, 2, sd.var)
  return(list(var = sum(var.phii),
         sd  = sqrt(sum(sd.var.phii^2))))
}
