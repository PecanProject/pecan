pr.samp <- function(distn,a,b,n) {
    do.call(paste('r',distn,sep=""),list(n,a,b))
}
