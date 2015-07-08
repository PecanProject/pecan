#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------


#--------------------------------------------------------------------------------------------------#
##' Fit a distribution to data
##'
##' @title Fit distribution to data  
##' @param trait.data data for distribution
##' @param dists list of distribution names
##' @return best fit distribution
##' @export
##' @author David LeBauer
#--------------------------------------------------------------------------------------------------#
fit.dist <- function(trait.data, trait = colnames(trait.data), 
                     dists = c('weibull', 'lognormal', 'gamma'), n = NULL) {
  if(class(trait.data) == 'data.frame') trait.data <- trait.data[,1]
  ## warning(immediate. = TRUE)
  nostart.dists <- dists[dists %in% c('weibull', 'lognormal', 'gamma', 'normal')]
  a <- lapply(nostart.dists, function(x) suppressWarnings(fitdistr(trait.data,x)))
  names(a) <- nostart.dists
  if('f' %in% dists){
    print(trait)
    if(trait == 'tt') {
      a[['f']] <- suppressWarnings(fitdistr(trait.data, 'f', start = list(df1=100, df2=200)))
    } else if (trait == 'sla') {
      a[['f']] <- suppressWarnings(fitdistr(trait.data, 'f', start = list(df1=6, df2=1)))
    } else if(trait == 'rrr') {
      a[['f']] <- suppressWarnings(fitdistr(trait.data, 'f', start = list(df1=6, df2=1)))
    } else if (trait == 'q'){
      a[['f']] <- suppressWarnings(fitdistr(trait.data, 'f', start = list(df1=1, df2=2)))
    }
  }
  if('beta' %in% dists){
    a[['beta']] <- suppressWarnings(fitdistr(trait.data, 'beta', 
                                             start = list(shape1 = 2, shape2 = 1 )))
  }
  aicvalues <- lapply(a, AIC)
  result <- t(sapply(dists, function(x) cbind(t(tabnum(a[[x]]$estimate)), 
                                              signif(aicvalues[[x]]))))
  colnames(result) <- c('a', 'b', 'AIC')
  print(result)
  bestfitdist <- names(which.min(aicvalues))
  parms <- tabnum(a[[bestfitdist]]$estimate)
  dist <- ifelse(bestfitdist == 'normal', 'norm', bestfitdist)
  return(data.frame(distribution = dist, 
                    a = as.numeric(parms[1]), 
                    b = as.numeric(parms[2]), 
                    n = ifelse(is.null(n), length(trait.data), n)))
} 
##' Prior fitting function for optimization
##'
##' This function is used within \code{\link{DEoptim}} to parameterize a distribution to the 
##' central tendency and confidence interval of a parameter. 
##' This function is not very robust; currently it needs to be tweaked when distributions
##' require starting values (e.g. beta, f)
##' @title prior.fn 
##' @param parms target for optimization
##' @param x vector with c(lcl, ucl, ct) lcl / ucl = confidence limits, ct = entral tendency 
##' @param alpha quantile at which lcl/ucl are estimated (e.g. for a 95% CI, alpha = 0.5)
##' @param distn named distribution, one of 'lnorm', 'gamma', 'weibull', 'beta'; support for other distributions not currently implemented 
##' @param central.tendency one of 'mode', 'median', and 'mean' 
##' @param trait name of trait, can be used for exceptions (currently used for trait == 'q')
##' @export
##' @return parms
##' @author David LeBauer
##' @examples
##' DEoptim(fn = prior.fn, 
##'                 lower = c(0, 0), 
##'                 upper = c(1000, 1000), 
##'                 x=c(2, 6, 3.3), 
##'                 alpha = 0.05, 
##'                 distn = 'lnorm')$optim$bestmem
#--------------------------------------------------------------------------------------------------#
prior.fn <- function(parms, x, alpha, distn, central.tendency = NULL, trait = NULL) {
  if(!distn %in% c('lnorm', 'gamma', 'weibull', 'beta')){
    stop(paste(distn, "not currently supported by prior.fn"))
  }
  if(distn == 'lnorm') {
    mu <- parms[1]
    sigma <- parms[2]         
    lcl <- mu + qnorm(alpha/2)*sigma
    ucl <- mu + qnorm(1-alpha/2)*sigma
    if(is.null(central.tendency)) {
      ct <- x[3]
    } else if (central.tendency == 'mean'){
      ct <-  mu - sigma^2
    } else if (central.tendency == 'median') {
      ct <- qlnorm(0.5, parms[1], parms[2])
    }
    x <- log(x)
  }
  if(distn == 'gamma'){
    lcl <- qgamma(alpha/2,   parms[1], parms[2])
    ucl <- qgamma(1-alpha/2, parms[1], parms[2])
    if(is.null(central.tendency)) {
      ct <- x[3]
    } else if(central.tendency == 'median'){
      ct <- qgamma(0.5, parms[1], parms[2])
    } else if (central.tendency == 'mean') {
      ct <- parms[1]/parms[2]
    } else if (central.tendency == 'mode') {
      ct <- ifelse (parms[1]>1, (parms[1]-1)/parms[2], 0)
    }
  }
  if(distn == 'weibull'){
    lcl <- qweibull(alpha/2,   parms[1], parms[2])
    ucl <- qweibull(1-alpha/2, parms[1], parms[2])
    if(is.null(central.tendency)) {
      ct <- x[3]
    } else if(central.tendency == 'median'){
      ct <- parms[2] * log(2)^(1/parms[1])
    } else if (central.tendency == 'mean') {
      ct <- parms[2] * gamma(1 +  1 / parms[2])
    } else if (central.tendency == 'mode') {
      stop("mode calculation not currently supported for weibull distribution")
    }
  }
  if (distn == 'beta') {
    a <- parms[1]
    if(central.tendency == 'mean' & trait == 'fineroot2leaf'){ ## fixed mean, optimize for a
      b <- a * (1/x[3] - 1)
    } else {
      b <- parms[2]
    }
    lcl <- qbeta(alpha/2,   a, b)  
    ucl <- qbeta(1-alpha/2, a, b) 
    if(is.null(central.tendency)) {
      ct <- x[3]
    } else if (central.tendency == 'mean'){
      ct <- a/(a+b)
    } else if (central.tendency == 'median'){
      ct <- qbeta(0.5, a, b)  ## median
    } else if (central.tendency == 'mode') {
      ct <- ifelse(a>1 & b>1,(a-1)/(a+b-2) , 0) ## mode
    } 
  } 
  return(sum(abs(c(lcl, ucl, ct) - x)))
}
#--------------------------------------------------------------------------------------------------#
##' Take n random samples from prior
##'
##' @title Sample from prior 
##' @param distn 
##' @param parama 
##' @param paramb 
##' @param n number of samples to return
##' @return vector with n random samples from prior
##' @export
##' @seealso \{code{\link{get.sample}}
#--------------------------------------------------------------------------------------------------#
pr.samp <- function(distn, parama, paramb, n) {
  do.call(paste('r', distn, sep=""), list(n, parama, paramb))
}
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##' Take n random samples from prior
##'
##' Like pr.samp, with prior as a single input
##' @title Get Samples
##' @param prior data.frame with distn, parama, paramb
##' @param n number of samples to return
##' @return vector with n random samples from prior
##' @seealso \link{pr.samp}
##' @export
#--------------------------------------------------------------------------------------------------#
get.sample <- function(prior, n) {
  if(prior$distn %in% c("exp","pois","geom")){  
    ## one parameter distributions
    do.call(paste('r', prior$distn, sep=""), list(n, prior$parama))
  } else {
    ## two parameter distributions
    do.call(paste('r', prior$distn, sep=""), list(n, prior$parama, prior$paramb))
  }
}
#==================================================================================================#

#--------------------------------------------------------------------------------------------------#
##' Calculates density at n points across the range of a parameter
##'
##' For a distribution and parameters, return the density for values ranging from alpha to 1-alpha 
##' @title Calculate densities
##' @param distn distribution
##' @param parama parameter
##' @param paramb parameter
##' @param n length of vector to be returned
##' @param alpha sets range at which the distribution will be evaluated (e.g. from alpha to 1-alpha)
##' @return dataframe with equally spaced x values and the corresponding densities
##' @export
##' @author David LeBauer
pr.dens <- function(distn, parama, paramb, n = 1000, alpha = 0.001) {
  alpha <- ifelse(alpha < 0.5, alpha, 1-alpha)
  n <- ifelse(alpha == 0.5, 1, n)
  range.x <- do.call(paste('q', distn, sep = ""), list(c(alpha, 1-alpha), parama, paramb))
  seq.x   <- seq(from = range.x[1], to = range.x[2], length.out = n)
  dens.df <- data.frame(x = seq.x,
                        y = do.call(paste('d', distn, sep=""),
                                    list(seq.x, parama, paramb)))
  return(dens.df)
}
#==================================================================================================#
##--------------------------------------------------------------------------------------------------#
##' Returns a data frame from \link{stats::density} function 
##'
##' @name create.density.df
##' @title Create Density Data Frame from Sample
##' @param samps a vector of samples from a distribution
##' @param zero.bounded 
##' @param distribution list with elements distn, parama, paramb,
##' e.g. \code{list('norm', 0, 1)}
##' @author David LeBauer
##' @export
##' @return data frame with x and y = dens(x)
##' @examples
##' prior.df <- create.density.df(distribution = list('norm',0,1))
##' plot(prior.df)
##' samp.df <- create.density.df(samps = rnorm(100))
##' lines(samp.df)
create.density.df <- function(samps = NULL,
                              zero.bounded = FALSE,
                              distribution = NULL,
                              n = 1000, ...) {
  samp.exists <- !is.null(samps)
  dist.exists <- !is.null(distribution)
  if(identical(samp.exists, dist.exists)){
    stop('create.density.df requires one and only one of:
         samps: a vector of samples, e.g. MCMC chain,
         OR
         distribution: a named distribution supported by R')
  }
  if(samp.exists){
    if(zero.bounded) {
      new.density <- zero.bounded.density(samps, n = 1000, ...)
    } else {    
      new.density <- density(samps, n = 1000, ...)
    }
    density.df <- with(new.density,
                       data.frame(x = x,
                                  y = y))
  }
  
  if(dist.exists) {
    density.df <- do.call(pr.dens, c(distribution[1:3]))
  }
  return(density.df)
}

####################################################################################################
### EOF.  End of R script file.          		
####################################################################################################
