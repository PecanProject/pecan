overlap.cdf.uncert <- function(x,y,iflog=T,quantile=c(0.05,0.95),fold=7) {
  load(PEcAn.qaqc)
  loc.x <- 1:length(x)
  for (f in 1:(fold-1)) {
    assign(paste("idx",f,sep=""),sample(loc.x,round(length(x)/fold,0)))
    loc.x <- loc.x[-which(match(loc.x,get(paste("idx",f,sep="")))>=1)]
  }
  assign(paste("idx",fold,sep=""),loc.x)
  loc.y <- 1:length(y)
  for (f in 1:(fold-1)) {
    assign(paste("idy",f,sep=""),sample(loc.y,round(length(y)/fold,0)))
    loc.y <- loc.y[-which(match(loc.y,get(paste("idy",f,sep="")))>=1)]
  }
  assign(paste("idy",fold,sep=""),loc.y)
  out <- vector()
  for (t in 1:fold) {
    out[t] <- overlap.cdf(x[-get(paste("idx",t,sep=""))],y[-get(paste("idy",t,sep=""))],iflog,quantile)
  }
  return(sd(out))
}