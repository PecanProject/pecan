##' Implementation of standard equations used to calculate mean and sd for a variety of 
##' named distributions different
##' 
##' @title Distribution Stats
##' @param distn named distribution, one of "beta", "exp", "f", "gamma", "lnorm", "norm", "t", 
##' @param a numeric; first parameter of \code{distn} 
##' @param b numeric; second parameter of \code{distn}
##' @return vector with mean and standard deviation
##' @export
##' @author David LeBauer
##' @examples
##' distn.stats("norm", 0, 1)
distn.stats <- function(distn,a,b){
  mean <- sd <- NULL
  if(distn == "beta"){
    mean <- a/(a+b)
    sd   <- sqrt(a*b/((a+b)^2 * (a+b+1)))
  } else if(distn == "exp"){
    mean <- 1/a
    sd <- 1/a
  } else if(distn == "f"){
    mean <- b/(b-2)
    sd   <- sqrt(2*b*b*(a + b -2)/(a*(b-2)^2*(b-4)))
  } else if(distn == "gamma"){
    mean <- a/b
    sd   <- sqrt(a/b^2)
  } else if (distn == "lnorm"){
    mean <- exp(a + 0.5*b^2)
    sd   <- sqrt(exp(2*a + b^2)*(exp(b^2)-1))
  } else if (distn == "norm"){
    mean <- a
    sd   <- b
  } else if (distn == "t"){
    mean <- 0
    sd   <- sqrt(a/(a-2))
  } else if (distn == "unif"){
    mean <- 0.5*(a+b)
    sd <- (b-a)/sqrt(12)
  } else if (distn == "weibull"){
    mean <- b * gamma(1+1/a)
    sd   <- b^2 * (gamma(1 + 2/a) - (gamma(1 + 1/a))^2)
  }
  return(c(mean, sd))
}

##' a helper function for computing summary statistics of a parametric distribution
##' 
##' @title return mean and standard deviation of a distribution for each distribution in a table with \code{colnames = c("distn", "a", "b"), e.g. in a table of priors
##' @param distns table of distributions; see examples 
##' @return named vector of mean and SD
##' @export
##' @author David LeBauer
distn.table.stats <- function(distns){
  y = as.data.frame(matrix(NA,nrow(distns),2))
  for(i in 1:nrow(distns)){
    x = distns[i,]
    y[i,] = distn.stats(x[1],as.numeric(x[2]),as.numeric(x[3]))
  }
  rownames(y) = rownames(distns)
  colnames(y) = c('mean', 'sd')
  return(y)
}

