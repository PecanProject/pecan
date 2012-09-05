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
#==================================================================================================#
#--------------------------------------------------------------------------------------------------#
##' Prior fitting function for optimization
##'
##' This function is used within \cite{\link{DEoptim}} to parameterize a distribution to the central tendency and confidence interval of a parameter. This function is not very robust; currently it needs to be tweaked when distributions require starting values (e.g. beta, f, 
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
#==================================================================================================#

