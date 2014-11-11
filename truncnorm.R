### Truncated Normal
dtnorm <- function(x, mu, s, Min=0, Max=+Inf){
  y <- dnorm(x, mu, s, log=TRUE) - log(pnorm(Max, mu, s) - pnorm(Min, mu, s))
  y[x < Min | x > Max] = -Inf
  return(y)
}

rtnorm <- function(n, mu, s, Min=0, Max=+Inf){
  x <- rnorm(n, mu, s)
  sel <- which(x < Min | x > Max)
  while(length(sel) > 0){
    x[sel] <- rnorm(length(sel), mu, s)
    sel <- which(x < Min | x > Max)
  }
  return(x)
}