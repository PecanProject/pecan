##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' This function can be used to determine if the 
##' algorithm from http://gis.stackexchange.com/questions/2/how-do-i-find-the-distance-between-two-coordinates/11#11
##' uses mean radius of the earth 6371 km = 3958 mi 
##' @title 
##' @param lat 
##' @param lon 
##' @param radius 
##' @param units 
##' @return 
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param this.lat 
##' @param this.lon 
##' @param that.lat 
##' @param that.lon 
##' @param max.distance
##' @param units 
##' @return logical: TRUE if coordinates are closer than max.distance

sites.inside.radius <- function(this.lat, this.lon, that.lat, that.lon, max.distance, units='km'){
  if (units == 'mi') {
    earth.radius <- 3958
  }
  if (units == 'km') {
    earth.radius <- 6371
  }
  
  xa <- cos(this.lat) * cos(this.long)
  ya <- cos(this.lat) * sin(this.long)
  za <- sin(this.lat)
  
  xb <- cos(that.lat) * cos(that.long)
  yb <- cos(that.lat) * sin(that.long)
  zb <- sin(that.lat)
  # calculate the great circle distance between the two using:
  if (units %in% c('mi', 'km')){
    distance <-  earth.radius * acos(xa * xb + ya * yb + za * zb)
  } else {
    stop('please use units of mi or km')
  }
  return(max.distance <= distance)
}
