site.lst <- function(site.id){
  
  dbparms <- list(driver="PostgreSQL" , user = "bety", dbname = "bety", password = "bety", host = "psql-pecan.bu.edu")
  con     <- db.open(dbparms)
  
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

