#' Truncated normal distribution functions
#' 
#' @name rtnorm
#' @title Random sampling from one-sided truncated normal distribution
#' @author Alexey Shiklomanov
#' @details Draws a random number and, if it doesn't fall within the 
#'      specified range, resample using an adjusted Normal CDF. This isn't
#'      performed immediately because CDF sampling calls three functions --
#'      qnorm, runif, and pnorm--and therefore is much less efficient than a
#'      simple random sample.
#' @param mu The mean parameter of the truncated normal. NOTE that this is NOT
#'      equal to the mean of the distribution
#' @param sd The standard deviation parameter of the truncated normal. Again, NOTE
#'      tht this is not the SD of the distribution.
#' @param MIN The minimum value, which defines the truncation.
#' @return Numeric length one.
rtnorm <- function(mu, sd, MIN){
    x <- rnorm(1, mu, sd)
    if(x < MIN)
        x <- qnorm(runif(1, pnorm(MIN, mu, sd, 1, 0), 1), mu, sd, 1, 0)
    return(x)
}

#' @name dtnorm
#' @title Truncated normal distribution density
#' @author Alexey Shiklomanov
#' @details Calculates the log density of a univariate truncated normal variable
#' @param x A random variable
#' @param mu The mean parameter of the distribution; NOTE this is not equal to the mean
#' @param sd The standard deviation parameter of the distribution
#' @param MIN Value at which the truncation takes place
#' @return Numeric; log density of the distribution, or -1e15 if the x < MIN
dtnorm <- function(x, mu, sd, MIN){
    if(x < MIN)
        return(-1e15)
    else
        return(dnorm(x, mu, sd, 1) - log(1-pnorm(MIN, mu, sd, 1, 0)))
}

