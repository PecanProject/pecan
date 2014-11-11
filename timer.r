## Loop timer

laptime <- function(tstart, i, n){
  tlap <- proc.time()
  tdif <- (tlap - tstart)[[3]]
  tleft <- tdif * (n/i - 1)
  cat(sprintf("\r%d of %d. Time elapsed: %.1f. Time remaining: %.1f", i, n, tdif, tleft))
}
