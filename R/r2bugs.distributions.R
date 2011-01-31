##' convert R parameterizations to BUGS paramaterizations
##' 
##' R and BUGS have different parameterizations for some distributions. This function transforms the distributions from R defaults to BUGS defaults. BUGS is an implementation of the BUGS language, and these transformations are expected to work for bugs.
##' @title convert R parameterizations to BUGS paramaterizations
##' @param priors data.frame with columns distn = distribution name, parama, paramb using R default parameterizations
##' @return priors dataframe using JAGS default parameterizations
##' @author David LeBauer

r2bugs.distributions <- function(priors) {

  sd2tau <- priors$distn %in% c('lnorm', 'norm')
  priors$paramb[sd2tau] <-  1/priors$paramb[sd2tau]^2

  scale2rate <- priors$dist %in% c('weibull')
  priors$paramb[scale2rate] <-  1 / priors$paramb[scale2rate]

  reverse.order <- priors$dist %in% c('binom')
  priors$paramb[reverse.order] <-  priors$parama[reverse.order]
  priors$parama[reverse.order] <-  priors$paramb[reverse.order]
  
  priors$distn[priors$distn == 'weibull'] <- 'weib'
  priors$distn[priors$distn == 'binom']   <- 'bin'
  priors$distn[priors$distn == 'chisq']   <- 'chisqr'
  priors$distn[priors$distn == 'nbinom']  <- 'negbin'

  return(priors)
}


##' @examples
##' priors <- data.frame(distn = c('weibull', 'lnorm', 'norm', 'gamma'),
##'                     parama = c(1, 1, 1, 1),
##'                     paramb = c(2, 2, 2, 2))
##' r2bugs.distributions(priors)
