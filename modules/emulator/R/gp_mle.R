##' @name gp_mle
##' @title gp_mle
##' @export
##'
##' @param theta proposed parameter vector: [mu, tauw, tauv, phi1...phiK]
##' @param d spatial distance matrix
##' @param nugget allows additional error in Y rather than fix interpolation to go through points
##' @param myY  vector of observed data
##' @param maxval maximum value
##' 
##' @return val
##' 
##' @author Michael Dietze
gp_mle <- function(theta, d, nugget, myY, maxval = Inf) {
  
  ## get parms
  mu   <- theta[1]
  tauw <- theta[2]
  if (tauw <= 0) {
    return(maxval)
  }
  i    <- 3
  tauv <- 0
  if (tauv < 0) {
    return(maxval)
  }
  if (nugget) {
    tauv <- theta[3]
    i <- 4
  }
  phi <- 1
  for (k in seq_along(theta)) {
    phi[k - i + 1] <- theta[k]
  }
  if (min(phi) <= 0) {
    return(maxval)
  }
  n <- length(myY)
  
  S <- calcSpatialCov(d, phi, tauw)
  
  val <- try(-sum(mvtnorm::dmvnorm(myY, rep(mu, n), S + diag(tauv, n), log = TRUE)))
  if (!is.numeric(val)) {
    return(maxval)
  }
  if (is.nan(val)) {
    return(maxval)
  }
  if (is.na(val)) {
    return(maxval)
  }
  return(val)
} # gp_mle


##' zero mean version
##' @title gp_mle2 
##' @export
##' 
##' @param theta proposed parameter vector: [mu, tauw, tauv, phi1...phiK]
##' @param d spatial distance matrix
##' @param nugget allows additional error in Y rather than fix interpolation to go through points
##' @param myY  vector of observed data
##' @param maxval maximum value
gp_mle2 <- function(theta, d, nugget, myY, maxval = Inf) {
  
  ## get parms
  tauw <- theta[1]
  if (tauw <= 0) {
    return(maxval)
  }
  i <- 2
  tauv <- 0
  if (tauv < 0) {
    return(maxval)
  }
  if (nugget) {
    tauv <- theta[2]
    i <- 3
  }
  phi <- 1
  for (k in seq_along(theta)) {
    phi[k - i + 1] <- theta[k]
  }
  if (min(phi) <= 0) {
    return(maxval)
  }
  n <- length(myY)
  
  S <- calcSpatialCov(d, phi, tauw)
  
  val <- try(-sum(mvtnorm::dmvnorm(myY, rep(0, n), S + diag(tauv, n), log = TRUE)))
  if (!is.numeric(val)) {
    return(maxval)
  }
  if (is.nan(val)) {
    return(maxval)
  }
  if (is.na(val)) {
    return(maxval)
  }
  return(val)
} # gp_mle2
