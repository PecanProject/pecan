##' Regrid dataset to even grid
##'
##' @title regrid
##' @param latlon.data dataframe with lat, lon, and some value to be regridded
##' @return dataframe with regridded data
##' @author David LeBauer
regrid <- function(latlon.data) {
  PEcAn.utils::need_packages("raster", "sp")
  ## from http://stackoverflow.com/a/15351169/513006
  spdf <- sp::SpatialPointsDataFrame(data.frame(x = latlon.data$lon, y = latlon.data$lat),
                                 data = data.frame(z = latlon.data$yield))
  ## Make evenly spaced raster, same extent as original data
  e <- raster::extent(spdf)
  ## Determine ratio between x and y dimensions
  ratio <- (e@xmax - e@xmin) / (e@ymax - e@ymin)

  ## Create template raster to sample to
  r  <- raster::raster(nrows = 56, ncols = floor(56 * ratio), ext = raster::extent(spdf))
  rf <- raster::rasterize(spdf, r, field = "z", fun = mean)

  # rdf <- data.frame( rasterToPoints( rf ) ) colnames(rdf) <-
  # colnames(latlon.data)
  arf <- as.array(rf)

  # return(rdf)
  return(arf)
} # regrid
