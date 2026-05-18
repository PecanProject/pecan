#' Look up California orchard density (trees per acre) by crop
#'
#' Returns the median trees per acre value for a California orchard crop,
#' derived from the DEMETER cropland biomass dataset. DEMETER photo dates
#' span 2010 to 2016, so modern high density plantings may be under
#' represented.
#'
#' Supported crops: Almonds, Walnuts, Oranges, Pistachios. Other crops
#' return NA with a warning.
#'
#' @param crop character. Crop name (case insensitive). One of "Almonds",
#'   "Walnuts", "Oranges", "Pistachios".
#'
#' @return integer scalar. Trees per acre for the requested crop. Returns
#'   NA_integer_ with a warning if the crop is not supported.
#'
#' @source Kroodsma, D. A., & Field, C. B. (2006). Carbon sequestration
#'   in California agriculture, 1980-2000. Ecological Applications,
#'   16(5), 1975-1985.
#'
#' @examples
#' tpa_lookup("Almonds")
#' tpa_lookup("walnuts")
#'
#' @export
tpa_lookup <- function(crop) {
  # median trees per acre by crop from DEMETER.
  # TODO: promote to a bundled age keyed dataset when per parcel
  # orchard age becomes available.
  medians <- c(
    almonds    = 80L,
    walnuts    = 41L,
    oranges    = 110L,
    pistachios = 119L
  )
  key <- tolower(crop)
  if (!key %in% names(medians)) {
    PEcAn.logger::logger.warn(
      "No DEMETER TPA available for crop '", crop, "'. ",
      "Supported: ", paste(names(medians), collapse = ", "), "."
    )
    return(NA_integer_)
  }
  medians[[key]]
}
