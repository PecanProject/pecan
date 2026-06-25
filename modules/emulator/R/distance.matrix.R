##' distance_matrix
##' @export
##'
##' @param x matrix of locations in physical or parameter space
##' @param power exponent used for calculating distance, default value of 2 = Pythagorean distance
##' @param dim dimension
##' 
##' @return symmetric matrix of pairwise squared distances
##' 
##' @author Michael Dietze
distance_matrix <- function(x, power = 1, dim = 2) {
  n <- nrow(x)
  d <- matrix(0, n, n)
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      d[i, j] <- sum((x[i, ] - x[j, ]) ^ power)
    }
  }
  return(d)
} # distance_matrix
