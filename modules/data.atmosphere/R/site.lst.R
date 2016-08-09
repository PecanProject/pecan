##' Find time zone for a site
##' 
##' @name site.lst
##' @title site.lst
##' @export
##' @param site.id
##' @param con
##' @author Betsy Cowdery
site.lst <- function(site.id, con){
  
  site <- db.query(paste("SELECT * from SITES where id =", site.id),con)
  
  if ("time_zone" %in% names(site) && !is.na(site[["time_zone"]]) && is.numeric(site$time_zone)){
    lst <- site$time_zone
  } else {
    site <- db.query(paste("SELECT ST_X(ST_CENTROID(geometry)) AS lon, ST_Y(ST_CENTROID(geometry)) AS lat FROM sites WHERE id =",site.id),con)
    require(geonames)
    options(geonamesUsername="carya")
    lst <- GNtimezone(site$lat, site$lon, radius = 0)$gmtOffset 
  }
  return(lst)
}
