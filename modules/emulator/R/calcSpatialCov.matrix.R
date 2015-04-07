`calcSpatialCov.matrix` <-
function(d,psi,tau){
  ##currently assumes an exponential spatial dependency
  ## can make gaussian by passing squared distance matrix
  ## d - spatial distance matrix
  ## psi - spatial corr
  ## tau - spatial var
  nl <- nrow(d)
  H <- matrix(0,nl,nl)
  for(i in 1:nl){
#    for(j in 1:nl){
#      H[i,j] <- tau*exp(-psi*d[i,j])
#    }
    for(j in 1:nl){
      H[i,] <- tau*exp(-psi*d[i,])
    }
  }
  return(H)
}

