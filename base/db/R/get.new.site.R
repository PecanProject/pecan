#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

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
        # No DB connection present. Generate a new ID using one of below steps:

        if (is.null(site$id) | is.na(site$id)) {
            if ((!is.null(site$lat) && !is.null(site$lon)) |
                (!is.na(site$lat) && !is.na(site$lon))
            ) {
                site.id <- generate_siteID(site$lat, site$lon)
                new.site <- data.frame(
                    id = as.numeric(site.id),
                    lat = site$lat,
                    lon = site$lon
                )
                str_ns <- paste0(new.site$lat, "-", new.site$lon)
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
                str_ns <- paste0(new.site$id %/% 1e+09, "-", new.site$id %% 1e+09)
                # We used a different str_ns as identifier here due to absence of lat/lon
            }
            # ID as well as lat/lon absent.
            # Return a WARN as we will be unable to identify such an instance due to lack of information.
            # We'll try to Generate a new ID similar to previous ones.
        } else {
            if ((!is.null(site$lat) && !is.null(site$lon)) |
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
            site.id <- generate_new_siteID()
            PEcAn.logger::logger.info(paste0("Generated siteID:", site.id))
            if ((!is.null(site$lat) && !is.null(site$lon)) |
                (!is.na(site$lat) && !is.na(site$lon))
            ) {
                new.site <- data.frame(
                    id = as.numeric(site.id),
                    lat = site$lat,
                    lon = site$lon
                )
                str_ns <- paste0(site$lat, "-", site$lon)
            } else {
                new.site <- data.frame(
                    id = site.id,
                    lat = NULL,
                    lon = NULL
                )
                str_ns <- paste0(new.site$id %/% 1e+09, "-", new.site$id %% 1e+09)
                # We used a different str_ns as identifier here due to absence of lat/lon
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
                str_ns <- paste0(site$lat, "-", site$lon)
            } else {
                latlon <- query.site(site$id, con = con)[c("lat", "lon")]
                new.site <- data.frame(
                    id = as.numeric(site$id),
                    lat = latlon$lat,
                    lon = latlon$lon
                )
                str_ns <- paste0(new.site$lat, "-", new.site$lon)
            }
        }
    }

    site.info <- list(new.site = new.site, str_ns = str_ns)

    return(site.info)
}

# Function to generate a normalized site ID using lat / lon
# Example usage
# siteID <- generate_siteID(-34.9284989, 138.6007456)
generate_siteID <- function(lat, lon) {
    # Normalize latitude and longitude
    norm_lat <- lat + 90 # Shift range to [0, 180]
    norm_lon <- lon + 180 # Shift range to [0, 360]

    # Scale normalized values (Assuming 4 digits each for lat and lon)
    scaled_lat <- as.integer((norm_lat / 180) * 9999) # Scaled latitude
    scaled_lon <- as.integer((norm_lon / 360) * 9999) # Scaled longitude

    # Create a unique ID by concatenating scaled values
    siteID <- as.integer(paste0(scaled_lat, scaled_lon))

    # Return the siteID
    return(siteID)
}

# Function to generate a new siteID (db-less runs ONLY)
generate_new_siteID <- function() {
    # Generate a random number. Assuming higher order integers to increase randomness in IDs
    random_id <- sample(10000:99999999, 1)
    return(as.numeric(random_id))
}
