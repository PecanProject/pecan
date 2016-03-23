#--------------------------------------------------------------------------------------------------#
##' 
##' @title lhc 
##' @export
##'

`lhc` <-
function(x,n.samp){
  ## Latin Hyper Cube
  ## simple uniform sampling w/ LHC permutation
  ## x <- list (n.dim x 2)
  ## n.samp <- number of samples
  n.dim <- nrow(x)
  samp <- permute <- matrix(runif(n.dim*n.samp),n.dim,n.samp)
  for(i in 1:n.dim) permute[i,] <- order(permute[i,])
  for(i in 1:n.dim) {
    myseq <- seq(x[i,1],x[i,2],length=n.samp+1)
    samp[i,] <- runif(n.samp,myseq[permute[i,]],myseq[permute[i,]+1])
  }
  t(samp)
}

