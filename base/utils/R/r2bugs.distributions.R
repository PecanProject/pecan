#' convert R parameterizations to BUGS paramaterizations
#'
#' R and BUGS have different parameterizations for some distributions. This function transforms the distributions from R defaults to BUGS defaults. BUGS is an implementation of the BUGS language, and these transformations are expected to work for bugs.
#'
#' @param priors data.frame with columns distn = distribution name, parama, paramb using R default parameterizations.
#' @param direction One of "r2bugs" or "bugs2r"
#' @return priors dataframe using JAGS default parameterizations
#' @author David LeBauer, Ben Bolker
#' @export
#' @examples
#' priors <- data.frame(distn = c('weibull', 'lnorm', 'norm', 'gamma'),
#'                      parama = c(1, 1, 1, 1),
#'                      paramb = c(2, 2, 2, 2))
#' r2bugs.distributions(priors)
r2bugs.distributions <- function(priors, direction = "r2bugs") {

  priors$distn  <- as.character(priors$distn)
  priors$parama <- as.numeric(priors$parama)
  priors$paramb <- as.numeric(priors$paramb)

  ## index dataframe according to distribution
  norm  <- priors$distn %in% c("norm", "lnorm")  # these have same tramsform
  weib  <- grepl("weib", priors$distn)  # matches r and bugs version
  gamma <- priors$distn == "gamma"
  chsq  <- grepl("chisq", priors$distn)  # matches r and bugs version
  bin   <- priors$distn %in% c("binom", "bin")  # matches r and bugs version
  nbin  <- priors$distn %in% c("nbinom", "negbin")  # matches r and bugs version

  ## Check that no rows are categorized into two distributions
  if (max(rowSums(cbind(norm, weib, gamma, chsq, bin, nbin))) > 1) {
    badrow <- rowSums(cbind(norm, weib, gamma, chsq, bin, nbin)) > 1
    stop(paste(unique(priors$distn[badrow])), "are identified as > 1 distribution")
  }

  exponent <- ifelse(direction == "r2bugs", -2, -0.5)
  ## Convert sd to precision for norm & lnorm
  priors$paramb[norm] <- priors$paramb[norm]^exponent
  if (direction == "r2bugs") {
    ## Convert R parameter b to BUGS parameter lambda by l = (1/b)^a
    priors$paramb[weib] <- (1/priors$paramb[weib]) ^ priors$parama[weib]
  } else if (direction == "bugs2r") {
    ## Convert BUGS parameter lambda to BUGS parameter b by b = l^(-1/a)
    priors$paramb[weib] <- priors$paramb[weib] ^ (-1 / priors$parama[weib])

  }
  ## Reverse parameter order for binomial and negative binomial
  priors[bin | nbin, c("parama", "paramb")] <- priors[bin | nbin, c("paramb", "parama")]

  ## Translate distribution names
  if (direction == "r2bugs") {
    priors$distn[weib] <- "weib"
    priors$distn[chsq] <- "chisqr"
    priors$distn[bin]  <- "bin"
    priors$distn[nbin] <- "negbin"
  } else if (direction == "bugs2r") {
    priors$distn[weib] <- "weibull"
    priors$distn[chsq] <- "chisq"
    priors$distn[bin]  <- "binom"
    priors$distn[nbin] <- "nbinom"
  }
  return(priors)
} # r2bugs.distributions


bugs2r.distributions <- function(..., direction = "bugs2r") {
  return(r2bugs.distributions(..., direction))
} # bugs2r.distributions


#' Sample from an R distribution using JAGS
#'
#' Takes a distribution with R parameterization, converts it to a
#' BUGS parameterization, and then samples from the distribution using
#' JAGS
#'
#' @param prior dataframe with distribution name and parameters
#' @param n.iter number of MCMC samples. Output will have n.iter/4 samples
#' @param n number of randomly chosen samples to return.
##    If NULL, returns all n.iter/4 of them
#' @return vector of samples
#' @export
#' @author David LeBauer
bugs.rdist <- function(prior = data.frame(distn = "norm", parama = 0, paramb = 1),
                       n.iter = 1e+05, n = NULL) {
  need_packages("rjags")
  if (!grepl("chisq", prior$distn)) {
    model.string <- paste0("model{Y ~ d", prior$distn, "(", prior$parama, ", ", prior$paramb, ")\n a <- x}")
  } else if (grepl("chisq", prior$distn)) {
    model.string <- paste0("model{Y ~ d", prior$distn, "(", prior$parama, ")\n a <- x}")
  } else {
    PEcAn.logger::logger.severe(paste("Unknown model.string", model.string))
  }

  writeLines(model.string, con = "test.bug")
  j.model <- rjags::jags.model(file = "test.bug", data = list(x = 1))
  mcmc.object <- stats::window(rjags::coda.samples(model = j.model,
                                     variable.names = c("Y"),
                                     n.iter = n.iter, thin = 2),
                        start = n.iter / 2)
  Y <- as.matrix(mcmc.object)[, "Y"]
  if (!is.null(n)) {
    Y <- sample(Y, n)
  }
  return(Y)
} # bugs.rdist
