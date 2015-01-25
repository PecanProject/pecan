##' For-loop timer

##' @name laptime
##' @title Lap timer for loops
##' @details {
##' Computes time remainng in loop and prints results to screen. Time remaining
##' is based on the average time elapsed for 'i' iterations.
##' }
##' 
##' @param tstart 'proc.time' object for beginning of loop.
##' @param i Iteration number
##' @param n Total number of iterations
##' @export
##' @author Alexey Shiklomanov

laptime <- function(tstart, i, n){
        ## Function for determining appropriate time units (sec, min, hr)
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
                return(sprintf("%.1f %s", out, unit))
        }
        tlap <- proc.time()
        tdif <- (tlap - tstart)[[3]]
        tdif.s <- time.unit(tdif)
        tleft <- tdif * (n/i - 1)
        tleft.s <- time.unit(tleft)
        cat(sprintf("\r%d of %d. Time elapsed: %s Time remaining: %s",
                    i, n, tdif.s, tleft.s))
}

