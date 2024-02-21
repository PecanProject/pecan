##' Find time zone for a site
##'
##' @export
##' @param site.id bety id of site to look up
##' @param con betydb connection object
##' @author Betsy Cowdery
site.lst <- function(site.id, con) {
  time.zone <- PEcAn.DB::db.query(paste("SELECT time_zone from SITES where id =", site.id), con)

  if (!is.na(time.zone) && !is.na(as.character(time.zone))) {
    lst <- PEcAn.utils::timezone_hour(time.zone)
  } else {
    site <- PEcAn.DB::db.query(paste("SELECT ST_X(ST_CENTROID(geometry)) AS lon, ST_Y(ST_CENTROID(geometry)) AS lat",
      "FROM sites WHERE id =", site.id), con)
    if (is.null(getOption("geonamesUsername"))) {
      options(geonamesUsername = "carya")
    }
    lst <- geonames::GNtimezone(site$lat, site$lon, radius = 0)$gmtOffset
  }
  return(lst)
} # site.lst
