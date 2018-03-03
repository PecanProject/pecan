#' Check ED2IN
#'
#' Check the basic structure of `ed2in` object, as well as consistency among 
#' arguments (e.g. run dates and coordinates are within the range of vegetation 
#' and meteorology data).
#'
#' @inheritParams write_ed2in
#' @export
check_ed2in <- function(ed2in) {
  # Check that required variables are set (not blank or have unfilled @@ tags)
  can_be_unset <- c(
    "LU_DATABASE",
    "SOIL_DATABASE",
    "IPHENYS1",
    "IPHENYSF",
    "IPHENYF1",
    "IPHENYFF"
  )
  unset <- !names(ed2in) %in% can_be_unset &
    (vapply(ed2in, function(x) all(is.na(x)), logical(1)) |
     grepl("@.*?@", ed2in))
  if (sum(unset) > 0) {
    PEcAn.logger::logger.severe(
      "The following required ED2IN tags are unset: ",
      paste(names(which(unset)), collapse = ", ")
    )
  }

  # Run dates fall within met dates
  met_object <- read_ed_metheader(ed2in[["ED_MET_DRIVER_DB"]])
  met_dates <- get_met_dates(met_object)
  ed2in_dates <- get_ed2in_dates(ed2in)

  outside_dates <- (!ed2in_dates %in% met_dates)
  if (sum(outside_dates) > 0) {
    PEcAn.logger::logger.severe(
      "The following run dates in ED2IN are not in met driver data: ",
      paste(ed2in_dates[outside_dates], collapse = ", ")
    )
  }

  # Run coordinates match vegetation prefix
  ed2in_lat <- ed2in[["POI_LAT"]]
  ed2in_lon <- ed2in[["POI_LON"]]
  veg_input <- read_ed_veg(ed2in[["SFILIN"]])
  if (veg_input$latitude != ed2in_lat) {
    PEcAn.logger::logger.severe(
      "ED2IN latitude ", ed2in_lat,
      " does not match vegetation input latitude ", veg_input$latitude
    )
  }
  if (veg_input$longitude != ed2in_lon) {
    PEcAn.logger::logger.severe(
      "ED2IN latitude ", ed2in_lon,
      " does not match vegetation input latitude ", veg_input$longitude
    )
  }

  # Run coordinates match meteorology drivers
  met_lat <- purrr::map_dbl(met_object, "ymin")
  met_dlat <- purrr::map_dbl(met_object, "dy") / 2
  met_lon <- purrr::map_dbl(met_object, "xmin")
  met_dlon <- purrr::map_dbl(met_object, "dx") / 2
  if (!any(between(ed2in_lat, met_lat - met_dlat, met_lat + met_dlat))) {
    PEcAn.logger::logger.severe(
      "ED2IN latitude ",
      ed2in_lat,
      " does not match meteorology latitudes ",
      paste(met_lat - met_dlat, met_lat + met_dlat, sep = " to ", collapse = ", ")
    )
  }
  if (!any(between(ed2in_lon, met_lon - met_dlon, met_lon + met_dlon))) {
    PEcAn.logger::logger.severe(
      "ED2IN latitude ",
      ed2in_lon,
      " does not match meteorology longitudes ",
      paste(met_lon - met_dlon, met_lon + met_dlon, sep = " to ", collapse = ", ")
    )
  }

  invisible(TRUE)
}

between <- function(x, a, b) {
  x >= a & x <= b
}
