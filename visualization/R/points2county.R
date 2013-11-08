##' convert a lat, lon, <value> dataset to county-level values
##'
##' Uses the \code{earth} function to fit a multivariate spline, and then
##' reproject to the (approximate) centroids of US counties, for counties that fall within the
##' lat / lon range of the data set 
##' @title Points To County
##' @param griddata a data frame with columns \code{lat, lon <some value>} or \code{. The function will assume 
##' @return a data.table with county-level information, including state and county name, state abbreviation,
##' lat, lon, county_fips, state_fips, fips, and interpolated output
##' @author David LeBauer
##' @export
##' @examples
##' data(ozone)
##' colnames(ozone) <- c("lon", "lat", "ozone")
points2county <- function(griddata){
  data(counties)
  X <- data.table(griddata)
#   setnames(X, c("lat", "lon", "" X)[colnames(X) %in% "x"] <- "lon"
#   colnames(X)[colnames(X) %in% "y"] <- "lat"

  stop(  "points2county not currently working")
  var <- colnames(X)[!colnames(X) %in% c("lat", "lon")]
#  if((length(var) > 1)) logger.error("too many columns")
#  if((length(var) < 1)) logger.error("no data in griddata, only ", colnames(griddata), "provided" )
     
#   if(X[,min(lat) < 19 | max(lat) > 68 | min(lon) < -174 | max(lon) > -67]) {
#     logger.warn("griddata includes points outside continental US; these will be removed")
#     X <- X[lat > 19 & lat < 68 & lon > -174 & lon < -67]
#   }
  
  setnames(X, old = var, new = "z")
  print(class(counties))
  print(colnames(X))
  cty.idx <- with(counties, lat < max(X$lat) & lat > min(X$lat) & 
                                lon < max(X$lon) & lon > min(X$lon))
  counties.subset <- counties[cty.idx,]
  xy <- data.frame(counties.subset[,list(lon, lat)])
  earth.model <- earth(z ~ lon * lat, data = X)
  pred  <- predict(earth.model, newdata = xy)
  result <- cbind(counties.subset, pred)
  setnames(result, old="z", new = var)
  return(result)
}