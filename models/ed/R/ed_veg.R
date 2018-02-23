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
  path_prefix_full <- paste0(path_prefix, ".", lat_prefix, lon_prefix)

  file_matches <- match_file(path_prefix_full, expect = 3)
  css_file <- match_file(path_prefix_full, suffix = "css", expect = 1)
  pss_file <- match_file(path_prefix_full, suffix = "pss", expect = 1)
  site_file <- match_file(path_prefix_full, suffix = "site", expect = 1)

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
  stopifnot(grepl(lat_rxp, fname))
  out <- as.numeric(gsub(lat_rxp, "\\1", fname))
  stopifnot(!is.na(out), length(out) != 1)
  out
}

#' Write ED inputs to directory
#'
#' Write a complete [ED inputs object][read_ed_veg] to disk. `css`, `pss`, and 
#' `site` files are automatically named and correctly formatted.
#'
#' @param ed_veg ED vegetation inputs object (see [read_ed_veg]).
#' @param path_prefix Desired full path and prefix
#' @return Full prefix for input files, suitable for `SFILIN` slot in ED2IN file.
#' @export
write_ed_veg <- function(ed_veg, path_prefix) {
  stop("Not functional yet")
  path_prefix_full <- paste0(
    path_prefix, ".",
    "lat", as.character(ed_veg$latitude),
    "lon", as.character(ed_veg$longitude)
  )
  base_name <- basename(path_prefix_full)
  dir_name <- dirname(path_prefix_full)
  dir.create(dir_name, showWarnings = FALSE)
  
  css_fname <- file.path(dir_name, paste0(base_name, ".css"))
  write.table(css, css_fname, quote = FALSE, row.names = FALSE)

  pss_fname <- file.path(dir_name, paste0(base_name, ".pss"))
  write.table(pss, pss_fname, quote = FALSE, row.names = FALSE)

  # TODO: Refactor
  site_fname <- file.path(dir_name, paste0(base_name, ".site"))
  #writelines(...)
  #write.table(...)
}

#' Match a file
#'
#' Return a list of files given a full prefix and optional suffix. Optionally, 
#' confirm that the right number of files are returned. If the wrong number of 
#' files is returned, throw an error.
#' @param path_prefix Full path and file prefix
#' @param suffix File suffix, as character (default = `NULL`)
#' @param expect Number of files expected to be returned (default = `NULL`)
#' @return Character vector of matched file names, as full paths.
match_file <- function(path_prefix, suffix = NULL, expect = NULL) {
  path <- dirname(path_prefix)
  prefix <- basename(path_prefix)
  file_matches <- list.files(path, prefix, full.names = TRUE)
  if (!is.null(suffix)) {
    file_matches <- grep(paste0(suffix, "$"), file_matches, value = TRUE)
  }
  if (!is.null(expect) && length(file_matches) != expect) {
    PEcAn.logger::logger.severe(
      "Expected ", expect, " files but found ", length(file_matches), ". ",
      "The following regular expression was used: ", file_rxp
    )
  }
  file_matches
}
