# iflog is T or F, quantile is a 2 dimension vector like c(0.05,0.95)
overlap.cdf <- function(x,y,iflog=T,quantile=c(0.05,0.95)){
  load(PEcAn.qaqc)
  area <- 0
  area.x <- 0
  area.y <- 0
  x <- na.omit(x)
  y <- na.omit(y)
  qx <- quantile(x,quantile)
  qy <- quantile(y,quantile)
  x <- x[(x>=qx[1])*(x<=qx[2])*(1:length(x))]
  y <- y[(y>=qy[1])*(y<=qy[2])*(1:length(y))]
  if (iflog==T) {
    x <- log(x+1)
    y <- log(y+1)
  }
  int <- sort(c(x,y))
  for (i in 2:length(int)){
    area <- area + min(sum(x<int[i])/length(x),sum(y<int[i])/length(y))*(int[i]-int[i-1])
    area.x <- area.x + sum(x<int[i])/length(x)*(int[i]-int[i-1])
    area.y <- area.y + sum(y<int[i])/length(y)*(int[i]-int[i-1])
  }
  n1 <- sum(x==max(int))+sum(y==max(int))
  area <- area + n1/(length(c(x,y))-n1)*(max(int)-min(int))
  area.x <- area.x + n1/(length(c(x,y))-n1)*(max(int)-min(int))
  area.y <- area.y + n1/(length(c(x,y))-n1)*(max(int)-min(int))
  ratio <- 2*area/(area.x+area.y)
  return(ratio)
}