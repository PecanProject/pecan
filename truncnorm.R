##' Truncated Normal distributions

##' @name dtnorm
##' @title Truncated normal distribution: density
##' @details {
##' Returns value of probability density function for the truncated normal distribution
##' with specified parameters
##' }
##' 
##' @param x Value for which to evaluate PDF
##' @param mu Mean of truncated normal
##' @param s Standard deviation of truncated normal
##' @param Min Hard lower bound of truncated normal (default=0)
##' @param Max Hard upper bound of truncated normal (default=+Inf)
##' @param log Return natural log of density value (default=TRUE)
##' @return Density value
##' @author Alexey Shiklomanov

dtnorm <- function(x, mu, s, Min=0, Max=+Inf, log=TRUE){
  y <- dnorm(x, mu, s, log=log) - log(pnorm(Max, mu, s) - pnorm(Min, mu, s))
  y[x < Min | x > Max] = -Inf
  return(y)
}

##' @name rtnorm
##' @title Truncated normal distribution: random value
##' @details {
##' Generates random value from truncated normal distribution with specified parameters.
##' }
##' 
##' @param n Number of values to generate
##' @param mu Mean of truncated normal
##' @param s Standard deviation of truncated normal
##' @param Min Hard lower bound of truncated normal (default=0)
##' @param Max Hard upper bound of truncated normal (default=+Inf)
##' @return Vector of 'n' random floats
##' @author Alexey Shiklomanov

rtnorm <- function(n, mu, s, Min=0, Max=+Inf){
  x <- rnorm(n, mu, s)
  sel <- which(x < Min | x > Max)
  while(length(sel) > 0){
    x[sel] <- rnorm(length(sel), mu, s)
    sel <- which(x < Min | x > Max)
  }
  return(x)
}
