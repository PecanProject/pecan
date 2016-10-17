##' Find time zone for a site
##' 
##' @name site.lst
##' @title site.lst
##' @export
##' @param site.id
##' @param con
##' @author Betsy Cowdery
site.lst <- function(site.id, con) {
  
  time.zone <- db.query(paste("SELECT time_zone from SITES where id =", site.id), con)
  if (!is.na(time.zone) && !is.na(as.numeric(time.zone))) {
    lst <- as.numeric(time.zone)
  } else {
    site <- db.query(paste("SELECT ST_X(ST_CENTROID(geometry)) AS lon, ST_Y(ST_CENTROID(geometry)) AS lat", 
      "FROM sites WHERE id =", site.id), con)
    options(geonamesUsername = "carya")
    library(geonames)
    lst <- GNtimezone(site$lat, site$lon, radius = 0)$gmtOffset
  }
  return(lst)
} # site.lst
