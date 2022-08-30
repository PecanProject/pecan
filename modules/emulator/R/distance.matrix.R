##' @name distance.martix
##' @title distance.matrix
##' @export
##'
##' @param x matrix of locations in physical or parameter space
##' @param power exponent used for calculating distance, default value of 2 = Pythagorean distance
##' @param dim dimenstion
##' 
##' @return d
##' 
##' @author Michael Dietze
distance.matrix <- function(x, power = 1, dim = 2) {
  n <- nrow(x)
  d <- matrix(0, n, n)
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      d[i, j] <- sum((x[i, ] - x[j, ]) ^ power)
      # d[i,j] <- 0 for(k in 1:dim){ d[i,j] <- d[i,j] + (x[i,k]-x[j,k])^power }
    }
  }
  return(d)
} # distance.matrix
