`distance` <-
function(x,power=1){
  dst <- list()
  n <- nrow(x); if(is.null(n)) n <- length(x)
  m <- ncol(x);
  if(is.null(m)) {
    m <- 1
    x <- matrix(x,n,m)
  }
  for(k in 1:m){
    dst[[k]] <- matrix(NA,n,n)
    for(i in 1:n){
      for(j in 1:n){
        dst[[k]][i,j] <- (abs(x[i,k]-x[j,k]))^power
      }
    }
  }
  dst
}

