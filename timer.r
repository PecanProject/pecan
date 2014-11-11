## Loop timer

laptime <- function(tstart, i, n){
  tlap <- proc.time()
  tdif <- time.unit((tlap - tstart)[[3]])
  tleft <- time.unit(tdif[[1]] * (n/i - 1))
  cat(sprintf("\r%d of %d. Time elapsed: %.1f. %s Time remaining: %.1f %s",
              i, n, tdif[[1]], tdif[[2]], tleft[[1]], tleft[[2]]))
}

time.unit <- function(x){
  if(x > 3600){
    out <- x/3600
    unit <- "hr"
  }
  else if(x > 60){
    out <- x/60
    unit <- "min"
  }
  else {
    out <- x
    unit <- "sec"
  }
  return(list(out, unit))
}