##' Simple interpolation of a density object to new points
##' 
##' @name predict.density
##' @title predict.density
##' @export
##' 
##' @param den density object
##' @param xnew new x coordinate
##' 
##' @return ynew
##'
##' @author Michael Dietze
predict.density <- function(den, xnew) {
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
