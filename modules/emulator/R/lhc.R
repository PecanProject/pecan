##' Latin Hyper Cube
##' 
##' Simple uniform sampling with LHC permutation
##' 
##' @name lhc
##' @title lhc
##' @export
##'
##' @param x <- list (n.dim x 2)
##' @param n.samp number of samples
##' 
##' @author Michael Dietze
lhc <- function(x, n.samp) {
    n.dim <- nrow(x)
    samp <- permute <- matrix(runif(n.dim * n.samp), n.dim, n.samp)
    for (i in seq_len(n.dim)) {
      permute[i, ] <- order(permute[i, ])
    } 
    for (i in seq_len(n.dim)) {
        myseq <- seq(x[i, 1], x[i, 2], length = n.samp + 1)
        samp[i, ] <- runif(n.samp, myseq[permute[i, ]], myseq[permute[i, ] + 1])
    }
    return(t(samp))
} # lhc

