#--------------------------------------------------------------------------------------------------#
##' 
##' @title distance.matrix 
##' @export
##'

`distance.matrix` <-
function(x,power=1,dim=2){
  n <- nrow(x)
  d <- matrix(0,n,n)
  for(i in 1:n){
    for(j in 1:n){
      d[i,j] <- sum((x[i,]-x[j,])^power)
#      d[i,j] <- 0
#      for(k in 1:dim){
#        d[i,j] <- d[i,j] + (x[i,k]-x[j,k])^power
#      }
    }
  }
  d
}

