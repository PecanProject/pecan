get.elevation <- function(lat, lon) {
  # http://stackoverflow.com/a/8974308/199217

  url  <- paste("http://www.earthtools.org/height", lat, lon, sep = "/")
  
  page <- RCurl::getURL(url)
  ans  <- XML::xmlTreeParse(page, useInternalNodes = TRUE)
  heightNode <- XML::xpathApply(ans, "//meters")[[1]]
  return(as.numeric(XML::xmlValue(heightNode)))
} # get.elevation


is.land <- function(lat, lon) {
  ncvar_get <- ncdf4::ncvar_get
  Lat  <- ncvar_get(nc = met.nc, varid = "lat")
  Lon  <- ncvar_get(nc = met.nc, varid = "lon")
  lati <- which.min(abs(Lat - lat))
  loni <- which.min(abs(Lon - lon))
  mask <- ncvar_get(nc = met.nc, varid = "mask", start = c(loni, lati), count = c(1, 1))
  return(mask >= 0)
} # is.land

get.latlonbox <- function(lati, loni, Lat = Lat, Lon = Lon) {
  lat <- c(mean(Lat[lati:(lati - 1)]), mean(Lat[lati:(lati + 1)]))
  lon <- c(mean(Lon[loni:(loni - 1)]), mean(Lon[loni:(loni + 1)]))
  return(c(sort(lat), sort(lon)))
} # get.latlonbox
