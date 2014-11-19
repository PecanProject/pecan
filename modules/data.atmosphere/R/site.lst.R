site.lst <- function(site.id,con){
  
  site <- db.query(paste("SELECT * from SITES where id =", site.id),con)

  if ("local_time" %in% names(site) && is.na(site[["local_time"]])==FALSE){
    lst <- site$local_time
  }else{
    site <- db.query(paste("SELECT ST_X(ST_CENTROID(geometry)) AS lon, ST_Y(ST_CENTROID(geometry)) AS lat FROM sites WHERE id =",site.id),con)
    require(geonames)
    options(geonamesUsername="ecowdery")
    lst <- GNtimezone(site$lat, site$lon, radius = 0)$dstOffset 
  }
  return(lst)
}

