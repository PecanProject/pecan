##' @name distance
##' @title distance
##' @export
##'
##' @param x
##' @param power
##'
##' @return dst
##'  
##' @author Michael Dietze
distance <- function(x, power = 1) {
  dst <- list()
  n <- nrow(x)
  if (is.null(n)) {
    n <- length(x)
  }
  m <- ncol(x)
  if (is.null(m)) {
    m <- 1
    x <- matrix(x, n, m)
  }
  for (k in seq_len(m)) {
    dst[[k]] <- matrix(NA, n, n)
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        dst[[k]][i, j] <- (abs(x[i, k] - x[j, k]))^power
      }
    }
  }
  return(dst)
} # distance
