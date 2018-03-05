#' Read ED2 vegetation inputs
#'
#' Read ED2 css, pss, and site files into a single ED input object.
#'
#' @param path_prefix Full path and prefix to initial condition files.
#' @param latitude Run latitude (default = `NULL`). If `NULL`, deduced from file name.
#' @param longitude Run longitude (default = `NULL`). If `NULL`, deduced from file name.
#' @param check Whether or not to check css, pss, and site files for validity.  
#' Default = `TRUE`.
#' @return List containing `css`, `pss`, and `site` objects, `latitude` and 
#' `longitude`, and `orig_paths`, a list of paths to the original `css`, `pss`, 
#' and `site` files.
#' @export
read_ed_veg <- function(path_prefix, latitude = NULL, longitude = NULL,
                        check = TRUE) {
  latlon_rxp <- "-?[[:digit:]]{1,3}(\\.[[:digit:]]*)?"
  if (!is.null(latitude)) {
    lat_prefix <- paste0("lat", as.character(latitude))
  } else {
    lat_prefix <- paste0("lat", latlon_rxp)
  }
  if (!is.null(longitude)) {
    lon_prefix <- paste0("lon", as.character(longitude))
  } else {
    lon_prefix <- paste0("lon", latlon_rxp)
  }
  path_prefix_full <- paste0(path_prefix, lat_prefix, lon_prefix)

  file_matches <- PEcAn.utils::match_file(path_prefix_full, expect = 3)
  css_file <- PEcAn.utils::match_file(path_prefix_full, suffix = "css", expect = 1)
  pss_file <- PEcAn.utils::match_file(path_prefix_full, suffix = "pss", expect = 1)
  site_file <- PEcAn.utils::match_file(path_prefix_full, suffix = "site", expect = 1)

  if (is.null(latitude)) {
    latitude <- get_latlon(css_file, "lat")
  }

  if (is.null(longitude)) {
    longitude <- get_latlon(css_file, "lon")
  }

  css <- read_css(css_file, check = FALSE)
  pss <- read_pss(pss_file, check = FALSE)
  site <- read_site(site_file, check = FALSE)

  if (check) {
    check_css(css, pss)
    check_pss(pss, site)
    check_site(site)
  }

  list(
    css = css,
    pss = pss,
    site = site,
    latitude = latitude,
    longitude = longitude,
    orig_paths = list(
      css = css_file,
      pss = pss_file,
      site = site_file
    )
  )
}

#' Parse latitude or longitude
#'
#' Automatically determine latitude or longitude from an ED input filepath. If 
#' the latitude/longitude regular expression isn't matched, this will throw an 
#' error.
#'
#' @param filepath Path to a css, pss, or site file
#' @param latlon Which value to retrieve, either "lat" for latitude or "lon" 
#' for longitude
#' @return Numeric value of latitude or longitude
get_latlon <- function(filepath, latlon) {
  stopifnot(latlon %in% c("lat", "lon"))
  fname <- basename(filepath)
  latlon_rxp <- "-?[[:digit:]]{1,3}(\\.[[:digit:]]*)?"
  rxp <- paste0(".*", latlon, "(", latlon_rxp, ").*")
  stopifnot(grepl(rxp, fname))
  out <- as.numeric(gsub(rxp, "\\1", fname))
  stopifnot(!is.na(out), length(out) == 1)
  out
}

#' Read individual css, pss, and site files
#'
#' Read files into objects usable by other PEcAn.ED2 utilities, and optionally check for errors.
#' @param filepath Full path to css, pss, or site file
#' @param check Logical. If `TRUE` (default), [check][check_css] that file is valid.
#' @param ... Additional arguments to [check functions][check_css].
#' @return `data.frame` containing
#' @export
read_css <- function(filepath, check = TRUE, ...) {
  css <- read.table(filepath, header = TRUE)
  if (check) {
    check_css(css, ...)
  }
  css
}

#' @rdname read_css
#' @export
read_pss <- function(filepath, check = TRUE) {
  pss <- read.table(filepath, header = TRUE)
  if (check) {
    check_pss(pss, ...)
  }
  pss
}

#' @rdname read_css
#' @export
read_site <- function(filepath, check = TRUE, ...) {
  top_line <- readLines(filepath, n = 1)
  nsite <- as.numeric(gsub(".*nsite +([[:digit:]]+).*", "\\1", top_line))
  file_format <- as.numeric(gsub(".*file_format +([[:digit:]]+).*", "\\1", top_line))
  site <- read.table(filepath, header = TRUE, skip = 1)
  attr(site, "nsite") <- nsite
  attr(site, "file_format") <- file_format
  if (check) {
    check_site(site, ...)
  }
  site
}

