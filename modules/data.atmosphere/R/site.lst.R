site.lst <- function(site.id){
  
  site <- db.query(paste("SELECT * from SITES where id =", site.id),con)
  
  if ("local_time" %in% names(site) && is.na(site[["local_time"]])==FALSE){
    lst <- site$local_time
  }else{
    require(geonames)
    options(geonamesUsername="ecowdery")
    lst <- GNtimezone(site$lat, site$lon, radius = 0)$dstOffset 
  }
  return(lst)
}

