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
##' @param site a dataframe with site information on lat, lon and time_zone
##' @param con Database connection object
##' latlon Global latlon abjoect which will store updated lat/lon
##' @return a dataframe with new site information on lat, lon and time_zone
##' @export
##' @author Abhinav Pandey
##'
##' @examples
##' get.new.site(site = data.frame(id = 1, lat = 40.1, lon = -88.2, time_zone = "UTC"), con = con)

get.new.site <- function(site, con, latlon = NULL) {

    # Check if site dataframe has an id column
    if (is.null(site$id)) {
        # Trying to stop current running process if no ID present.
        # Currently, i want to replace this with a helper function which will check for the ID
        ## If ID is not present, the helper function will generate a new one.
        PEcAn.logger::logger.error("Site dataframe does not have an id column")
    }

    # setup site database number, lat, lon and name and copy for format.vars if new input
    if (is.null(site$lat) | is.null(site$lon)) {
        latlon <- query.site(site$id, con = con)[c("lat", "lon")]
        new.site <- data.frame(
            id = as.numeric(site$id),
            lat = latlon$lat,
            lon = latlon$lon
        )
        str_ns <- paste0(new.site$lat, "-", new.site$lon)
    } else {
        new.site <- data.frame(
            id = as.numeric(site$id),
            lat = site$lat,
            lon = site$lon
        )
        str_ns <- paste0(site$lat, "-", site$lon)
    }

    site.info <- list(new.site = new.site, str_ns = str_ns)

    return (site.info)
}
