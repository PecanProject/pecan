calcSpatialCov.matrix <- function(d, psi, tau, ...) {
    return(tau * exp(-psi * d))
  }
