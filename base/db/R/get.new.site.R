##' Get new site info using provided site information
##'
##' @title Get New Site Info
##' @param site a dataframe with site information including id, lat, lon, and time_zone.
##' @param con Database connection object.
##' @param latlon Optional global latlon object which will store updated lat/lon.
##' @return a dataframe with new site information on lat, lon and time_zone
##' @export
##' @author Abhinav Pandey
##'
##' @examples
##' get.new.site(site=data.frame(id=1,lat=40.1,lon=-88.2,time_zone="UTC"),con=NULL,latlon=NULL)

get.new.site <- function(site, con = NULL, latlon = NULL) {
    if (is.null(con)) {
        PEcAn.logger::logger.debug("DB connection is closed. Trying to generate a new site ID or use pre-existing one.")
        # No DB connection present. Generate a new ID using one of below steps:

        if (is.null(site$id) | is.na(site$id)) {
            if ((!is.null(site$lat) && !is.null(site$lon)) &&
                (!is.na(site$lat) && !is.na(site$lon))
            ) {
                site.id <- paste0(lat, "_", lon)
                new.site <- data.frame(
                    id = as.numeric(site.id),
                    lat = site$lat,
                    lon = site$lon
                )
                str_ns <- paste0(new.site$lat, "_", new.site$lon)
            } # lat/lon present but ID is absent
            # Use Pre-existing normalised lat/lon
            else {
                PEcAn.logger::logger.warn("Site dataframe does not have an id column")
                site.id <- generate_new_siteID()
                PEcAn.logger::logger.info(paste0("Generated siteID:", site.id))
                # Optionally, create a new site data frame with the random ID
                new.site <- data.frame(
                    id = site.id,
                    lat = NULL,
                    lon = NULL
                )
                str_ns <- paste0(new.site$id %/% 1e+09, "_", new.site$id %% 1e+09)
                # We used a different str_ns as identifier here due to absence of lat/lon
            }
            # ID as well as lat/lon absent.
            # Return a WARN as we will be unable to identify such an instance due to lack of information.
            # We'll try to Generate a new ID similar to previous ones.
        } else {
            if ((!is.null(site$lat) && !is.null(site$lon)) &&
                (!is.na(site$lat) && !is.na(site$lon))
            ) {
                new.site <- data.frame(
                    id = as.numeric(site$id),
                    lat = site$lat,
                    lon = site$lon
                )
                str_ns <- paste0(new.site$lat, "-", new.site$lon)
            } # siteId as well as lat/lon present
            else {
                new.site <- data.frame(
                    id = site$id,
                    lat = NULL,
                    lon = NULL
                )
                str_ns <- paste0(new.site$id %/% 1e+09, "-", new.site$id %% 1e+09)
            } # siteId present but lat/lon absent
        }
    } else {
        # Check if site dataframe has an id column
        if (is.null(site$id) | is.na(site$id)) {
            PEcAn.logger::logger.warn("Site dataframe does not have an id column. Generating a unique ID")
            if ((!is.null(site$lat) && !is.null(site$lon)) &&
                (!is.na(site$lat) && !is.na(site$lon))
            ) {
                PEcAn.logger::logger.info(paste0("Generated siteID using lat and lon:", site.id))
                site.id <- generate_new_siteID(site$lat, site$lon)
                new.site <- data.frame(
                    id = as.numeric(site.id),
                    lat = site$lat,
                    lon = site$lon
                )
                str_ns <- paste0(site$lat, "-", site$lon)
            } else {
                PEcAn.logger::logger.severe("Missing site-id, site lat & site-lon. Stopping the process due to lack of information")
            }
        } else {
            # setup site database number, lat, lon and name and copy for format.vars if new input
            if ((!is.null(site$lat) && !is.null(site$lon)) |
                (!is.na(site$lat) && !is.na(site$lon))
            ) {
                new.site <- data.frame(
                    id = as.numeric(site$id),
                    lat = site$lat,
                    lon = site$lon
                )
                str_ns <- paste0(site$lat, "_", site$lon)
            } else {
                latlon <- query.site(site$id, con = con)[c("lat", "lon")]
                new.site <- data.frame(
                    id = as.numeric(site$id),
                    lat = latlon$lat,
                    lon = latlon$lon
                )
                str_ns <- paste0(new.site$lat, "_", new.site$lon)
            }
        }
    }

    site.info <- list(new.site = new.site, str_ns = str_ns)

    return(site.info)
}


# Function to generate a new siteID using hashing (db-less runs ONLY)
generate_new_siteID <- function(lat, lon) {
    latlon_str <- paste0(round(lat, 8), round(lon, 8))
    hash <- openssl::sha256(latlon_str)
    
    # Extracting first 10 characters of hash as a UID
    uid <- substr(as.character(hash), 1, 10)
    return(uid)
}
