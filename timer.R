## Loop timer

laptime <- function(tstart, i, n){
  tlap <- proc.time()
  tdif <- (tlap - tstart)[[3]]
  tdif.s <- time.unit(tdif)
  tleft <- tdif * (n/i - 1)
  tleft.s <- time.unit(tleft)
  cat(sprintf("\r%d of %d. Time elapsed: %s Time remaining: %s",
              i, n, tdif.s, tleft.s))
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
  return(sprintf("%.1f %s", out, unit)
}
