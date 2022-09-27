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
  if (!requireNamespace("maptools", quietly = TRUE)) {
    PEcAn.logger::logger.error(
      "PEcAn.data.land::find.land needs package `maptools`.",
      "Please install it and try again.")
  }
  # If maptools used lazy data, this could just be `maptools::wrld_simpl`...
  wrld_simpl <- NULL
  utils::data("wrld_simpl",package="maptools",envir = environment())
  
  ## Create a SpatialPoints object
  points <- expand.grid(lon, lat)
  colnames(points) <- c("lat", "lon")
  pts <- sp::SpatialPoints(points, proj4string = sp::CRS(sp::proj4string(wrld_simpl)))
  
  ## Find which points fall over land
  landmask <- cbind(points, data.frame(land = !is.na(sp::over(pts, wrld_simpl)$FIPS)))
  if (plot) {
    plot(wrld_simpl)
    landmask[, points(lon, lat, col = 1 + land, pch = 16)]
  }
  return(landmask)
} # find.land
