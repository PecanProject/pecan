## from Josh O'Brien http://stackoverflow.com/a/19148078/513006

##' find land
##'
##' extract terrestrial lat and lon coordinates from a grid
##'
##' @title find.land
##' @param lat vector of latitudes
##' @param lon vector of longitudes
##' @return data frame with numeric lat, lon and logical value 'land'
##' @export
##' @author David LeBauer
find.land <- function(lat, lon, plot = FALSE) {
  data("wrld_simpl",package="maptools",envir = environment())
  
  ## Create a SpatialPoints object
  points <- expand.grid(lon, lat)
  colnames(points) <- c("lat", "lon")
  pts <- sp::SpatialPoints(points, proj4string = sp::CRS(sp::proj4string(wrld_simpl)))
  
  ## Find which points fall over land
  landmask <- cbind(points, data.frame(land = !is.na(over(pts, wrld_simpl)$FIPS)))
  if (plot) {
    plot(wrld_simpl)
    landmask[, points(lon, lat, col = 1 + land, pch = 16)]
  }
  return(landmask)
} # find.land
