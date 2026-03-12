##' Currently assumes an exponential spatial dependency
##'
##' Can make gaussian by passing squared distance matrix
##'
##' @param d spatial distance matrix
##' @param psi spatial corr
##' @param tau spatial var
##' @param ... additional arguments (currently unused)
##' @return spatial covariance matrix
##' @author Michael Dietze
##' @export
calcSpatialCov.matrix <- function(d, psi, tau, ...) {
  nl <- nrow(d)
  H  <- matrix(0, nl, nl)
  for (i in seq_len(nl)) {
    # for(j in 1:nl){ H[i,j] <- tau*exp(-psi*d[i,j]) }
    for (j in seq_len(nl)) {
      H[i, ] <- tau * exp(-psi * d[i, ])
    }
  }
  return(H)
}
