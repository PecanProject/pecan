closest_xy = function(slat, slon,infolder,infile){
  require(ncdf4)
  
  test.file = dir(infolder,infile,full.names=TRUE)
  test.file = test.file[grep("*.nc",test.file)]
  if(length(test.file)==0) return(NULL)
  test.file = test.file[1]
  
  nc <- nc_open(test.file)
  lat  <- ncvar_get(nc,"lat")
  lon  <- ncvar_get(nc,"lon")
  
  if (all(dim(lat) == dim(lon))){
    rows <- nrow(lat)
    cols <- ncol(lat)
    D <- matrix(-1,rows,cols)
    
    for (i in 1:rows){
      for (j in 1:cols){
        tlat <- lat[i,j]
        tlon <- lon[i,j]
        c1 <- tlat >= 20
        c2 <- tlat <= 50
        c3 <- tlon >= -125
        c4 <- tlon <= -65
        if (c1 & c2 & c3 & c4){
          D[i,j] <- sqrt( (tlat - slat)^2 + (tlon - slon)^2 )
        }
      }
    }
  }
  
  dmin <- min(D[which(D>0)])
  xy <- which(D == dmin, arr.ind=T)
  
  if (nrow(xy) > 1){
    print("More than one possible coordinate, choosing first one")
  }
  return(list(x = as.numeric(xy[1,1]), y = as.numeric(xy[1,2])))
}

