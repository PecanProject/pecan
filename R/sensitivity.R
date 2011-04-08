##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param quantiles.input 
##' @param quantiles.output 
##' @return spline function from splinefun that estimates univariate response of parameter input to model output  
##' @author David
sa.spline <- function(quantiles.input, quantiles.output){
  newfn <- splinefun(quantiles.input, quantiles.output, method = "monoH.FC")
  return(newfn)
}

spline.estimate <- function(sa.splinefun, trait.samples) {
  y <- sa.splinefun(trait.samples)
  return(y)
}


sd.var <- function(x){
  var(x)^2*(2/(length(x)-1) + kurtosis(x)/length(x))
}
sensitivity <- function(sa.splinefuns, traitmean){
  sensitivity <- sa.splinefuns(traitmean, 1)
}

zero.truncate <- function(y) {
  y[y<0 | is.na(y)] <- 0
  return(y)
}


