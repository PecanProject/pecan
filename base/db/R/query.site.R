##' Given site_id, return site table
##'
##' @param con : database connection
##' @param site.id The id of the site
##' @export query.site
##'
##' @author Betsy Cowdery
##'
query.site <- function(site.id,con){
  site <- db.query(
    query = paste(
      "SELECT *, ST_X(ST_CENTROID(geometry)) AS lon, ST_Y(ST_CENTROID(geometry))
        AS lat FROM sites WHERE id =", site.id
    ),
    con = con
  )
  if (nrow(site)==0) {
    PEcAn.logger::logger.error("Site not found"); return(NULL)
  }
  if (!(is.na(site$lon)) && !(is.na(site$lat))) {
    return(site)
  } else {
    return(NULL)
  }
}
