get.elevation <- function(lat, lon){
  # http://stackoverflow.com/a/8974308/199217
  library(RCurl)
  library(XML)
  
  url <- paste("http://www.earthtools.org/height", lat, lon, sep = "/")
  
  page <- getURL(url)
  ans <- xmlTreeParse(page, useInternalNodes = TRUE)
  heightNode <- xpathApply(ans, "//meters")[[1]]
  (height <- as.numeric(xmlValue(heightNode)))  
}

get.soil <- function(lat, lon, soil.nc = soil.nc){
  
  ## Lat and Lon
  Lat <- ncvar_get(soil.nc, "lat")
  Lon <- ncvar_get(soil.nc, "lon")
  
  lati <- which.min(abs(Lat - lat))
  loni <- which.min(abs(Lon - lon))
  
  ## topsoil
  usda_class <- ncvar_get(soil.nc, "t_usda_tex",
                          start = c(loni, lati),
                          count = c(1,1))
  ref_depth <- ud.convert(ncvar_get(soil.nc, "ref_depth",
                                    start = c(loni, lati),
                                    count = c(1, 1)), "cm", "m")
  return(list(usda_class = usda_class, ref_depth = ref_depth))
}

is.land <- function(lat, lon){
  Lat <- ncvar_get(nc = met.nc, varid = "lat")
  Lon <- ncvar_get(nc = met.nc, varid = "lon")
  lati <- which.min(abs(Lat-lat))
  loni <- which.min(abs(Lon-lon))
  mask <- ncvar_get(nc = met.nc, varid = "mask",
                    start = c(loni, lati), count = c(1,1))
  return(mask >= 0)
}

get.latlonbox <- function(lati, loni, Lat = Lat, Lon = Lon){
  lat <- c(mean(Lat[lati:(lati-1)]), mean(Lat[lati:(lati+1)]))
  lon <- c(mean(Lon[loni:(loni-1)]), mean(Lon[loni:(loni+1)]))
  return(c(sort(lat), sort(lon)))
}