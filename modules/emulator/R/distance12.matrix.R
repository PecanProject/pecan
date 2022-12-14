##' @name distance12.martix
##' @title distance12.matrix
##' @export
##'
##' @param x matrix of locations in parameter space
##' @param n1 number of rows in the original dataset
##' @param power exponent used for calculating distance, default value of 2 = Pythagorean distance
##' 
##' @return d
##' 
##' @author Michael Dietze
distance12.matrix <- function(x, n1, power = 1) {
  n <- nrow(x)
  d <- matrix(0, n, n - n1)
  sel <- (n1 + 1):n
  for (i in seq_len(n)) {
    d[i, ] <- (x[i, 1] - x[sel, 1]) ^ power + (x[i, 2] - x[sel, 2]) ^ power
  }
  return(d)
} # distance12.matrix
