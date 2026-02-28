##' Simple interpolation of a density object to new points
##'
##' @param object a \code{density} object (as returned by \code{\link[stats]{density}})
##' @param xnew numeric vector of new x coordinates at which to evaluate the density
##' @param ... additional arguments (currently unused)
##' @return numeric vector of interpolated density values at \code{xnew}
##' @author Michael Dietze
##' @export
predict.density <- function(object, xnew, ...) {
  den <- object
  neval <- length(den$x)
  nnew  <- length(xnew)
  ynew  <- rep(NA, nnew)
  for (i in seq_len(nnew)) {
    j <- findInterval(xnew[i], den$x)
    if (j == 0 || j == neval) {
      ynew[i] <- 0  ## don't extrapolate beyond range,set to 0
    } else {
      ynew[i] <- den$y[j] + (den$y[j + 1] - den$y[j]) / 
        (den$x[j + 1] - den$x[j]) * 
        (xnew[i] - den$x[j])
    }
  }
  return(ynew)
} # predict.density
