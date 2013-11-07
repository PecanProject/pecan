## from Josh O'Brien http://stackoverflow.com/a/19148078/513006

library(maptools)
data(wrld_simpl)
library(data.table)
load("/home/dlebauer/met/ncep/latlon.RData")

## Create a SpatialPoints object
points <- data.table(expand.grid(Lon-180, Lat))
setnames(points, c("lon", "lat"))
pts <- SpatialPoints(points, proj4string=CRS(proj4string(wrld_simpl)))

## Find which points fall over land
landmask <- cbind(points, data.table(land = !is.na(over(pts, wrld_simpl)$FIPS)))

## Check that it worked
plot(wrld_simpl) 
landmask[,points(lon, lat, col=1+land, pch=16)]
ncep_landmask <- landmask
setkeyv(landmask, c("lat", "lon"))
save(Lat, Lon, landmask, file = "../data/ncep_landmask.RData")
