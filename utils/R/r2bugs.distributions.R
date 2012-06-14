##' convert R parameterizations to BUGS paramaterizations
##' 
##' R and BUGS have different parameterizations for some distributions. This function transforms the distributions from R defaults to BUGS defaults. BUGS is an implementation of the BUGS language, and these transformations are expected to work for bugs.
##' @title convert R parameterizations to BUGS paramaterizations
##' @param priors data.frame with columns distn = distribution name, parama, paramb using R default parameterizations
##' @return priors dataframe using JAGS default parameterizations
##' @author David LeBauer
##' @export
##' @examples
##' priors <- data.frame(distn = c('weibull', 'lnorm', 'norm', 'gamma'),
##'                      parama = c(1, 1, 1, 1),
##'                      paramb = c(2, 2, 2, 2))
##' r2bugs.distributions(priors)
r2bugs.distributions <- function(priors) {

  norm   <- priors$distn %in% 'norm'
  lnorm  <- priors$distn %in% 'lnorm'
  weib   <- priors$distn %in% 'weibull'
  gamma  <- priors$distn %in% 'gamma'
  bin    <- priors$distn %in% 'binom'
  chisqr <- priors$distn %in% 'chisq'
  negbin <- priors$distn %in% 'nbinom'
  
  ## Convert sd to precision for norm & lnorm
  priors$paramb[norm | lnorm] <-  signif(1/priors$paramb[norm | lnorm]^2, 4)
  ## Convert R parameter b to JAGS parameter lambda by l = (1/b)^a
  priors$paramb[weib] <-   signif(1 / priors$paramb[weib]^priors$parama[weib], 4)
  ## Reverse parameter order for binomial
  priors[bin, c('parama', 'paramb')] <-  priors[bin, c('parama', 'paramb')]
  ## Convert Gamma rate to scale parameter
  priors[gamma, 'paramb'] <-  1 / priors[gamma, 'paramb']
  
  ## Translate distribution names
  priors$distn <- gsub('weibull', 'weib',
                       gsub('binom', 'bin',
                            gsub('chisq', 'chisqr',
                                 gsub('nbinom', 'negbin',
                                      as.vector(priors$distn)))))
  return(priors)
}
