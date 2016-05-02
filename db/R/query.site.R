##' @name query.site
##' @title Given site_id, return site table
##' @param site_id
##' @param con : database connection
##' @export 
##' 
##' @author Betsy Cowdery 
##' 
query.site <- function(site.id,con){
  site <- db.query(paste("SELECT *, ST_X(ST_CENTROID(geometry)) AS lon, ST_Y(ST_CENTROID(geometry))
                         AS lat FROM sites WHERE id =",site.id),con)
  if(nrow(site)==0){logger.error("Site not found"); return(NULL)}
  if(!(is.na(site$lat)) && !(is.na(site$lat))){
    return(site)
  }
}