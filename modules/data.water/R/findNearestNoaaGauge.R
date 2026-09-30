#' Find the nearest NOAA gauge to each site
#'
#' Finds the nearest long term NOAA gauge to each input site using
#' `sf::st_nearest_feature()`, joins the corresponding gauge attributes to
#' the site table, and calculates the straight-line distance between the site
#' and gauge.
#'
#' `sites` may be data frame containing latitude and
#' longitude columns or existing `sf` objects.
#'
#' @param sites A data frame, tibble, or `sf` object containing site locations.
#' @param site_lat Character. Name of the latitude column in `sites`.
#'   Default is `"latitude"`.
#' @param site_lon Character. Name of the longitude column in `sites`.
#'   Default is `"longitude"`.
#'
#' @return An data.frame object containing the original site attributes and geometry,
#'   attributes of the nearest gauge, and distance to the gauge in meters and
#'   kilometers.
#'
#' @details
#' Distances are straight-line geographic distances. For coastal and estuarine
#' applications, the nearest gauge by straight-line distance may not
#' necessarily be the most hydrologically connected gauge. This also won't work outside the U.S.
#'
#' @export
findNearestNoaaGauge <- function(
    sites,
    site_lat = "latitude",
    site_lon = "longitude"
) {
  
  # Convert sites to sf if needed
  if (!inherits(sites, "sf")) {
    
    missing_cols <- setdiff(
      c(site_lat, site_lon),
      names(sites)
    )
    
    if (length(missing_cols) > 0) {
      stop(
        "Sites are missing required coordinate column(s): ",
        paste(missing_cols, collapse = ", ")
      )
    }
    
    sites <- sf::st_as_sf(
      sites,
      coords = c(site_lon, site_lat),
      crs = 4326,
      remove = FALSE
    )
  } else {
    sites <- sf::st_transform(sites, crs = 4326)
  }
  
  gauges_tab <- readr::read_csv(
    system.file(
      "extdata",
      "noaa_us_gauge_info.csv",
      package = "data.water"
    ),
    show_col_types = F
  )
  
  
  # Put gauges in the same CRS as sites
  gauges <- sf::st_as_sf(gauges_tab,
                         coords = c("longitude", "latitude"),
                         crs = 4326,
                         remove = FALSE
                         )
  
  # Find nearest gauge
  nearest_idx <- sf::st_nearest_feature(
    sites,
    gauges
  )
  
  # Calculate distance to matched gauge
  distance_m <- sf::st_distance(
    sites,
    gauges[nearest_idx, ],
    by_element = TRUE
  )
  
  # Get attributes for matched gauges
  gauge_attrs <- gauges[nearest_idx, ] |>
    sf::st_drop_geometry()
  
  # Prefix gauge columns that conflict with site column names
  duplicate_names <- intersect(
    names(gauge_attrs),
    names(sites)
  )
  
  if (length(duplicate_names) > 0) {
    gauge_attrs <- gauge_attrs |>
      dplyr::rename_with(
        ~ paste0("gauge_", .x),
        dplyr::all_of(duplicate_names)
      )
  }
  
  # Join gauge information to sites
  sites_output <- dplyr::bind_cols(
    sites,
    gauge_attrs
  ) |>
    dplyr::mutate(
      distance_km = as.numeric(distance_m) / 1000) %>% 
    sf::st_drop_geometry() %>% 
    tidyr::as_tibble() %>% 
    rename(gauge_id = noaa_id,
           gauge_name = noaa_name
           )
  
  return(sites_output)
    
}
