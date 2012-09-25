`nderiv` <-
function(x,y){
  der <- 0 * x
  n <- length(y)
  if(n < 2) return(der)
  der[1] <- (y[2]-y[1])/(x[2]-x[1])
  der[n] <- (y[n]-y[n-1])/(x[n]-x[n-1])
  if(n == 2) return(der)
  for(i in 2:(n-1)){
    der[i] <- (y[i+1]-y[i-1])/(x[i+1]-x[i-1])
  }
  return(der)
}

