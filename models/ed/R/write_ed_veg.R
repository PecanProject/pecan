#' Write ED inputs to directory
#'
#' Write a complete [ED inputs object][read_ed_veg] to disk. `css`, `pss`, and 
#' `site` files are automatically named and correctly formatted.
#'
#' @param ed_veg ED vegetation inputs object (see [read_ed_veg]).
#' @param path_prefix Desired path and prefix (without latitude and longitude)
#' @return Named list (`css`, `pss`, `site`) of full file paths, invisibly
#' @export
write_ed_veg <- function(ed_veg, path_prefix) {
  css_file <- write_css(ed_veg$css, path_prefix, ed_veg$latitude, ed_veg$longitude)
  pss_file <- write_pss(ed_veg$pss, path_prefix, ed_veg$latitude, ed_veg$longitude)
  site_file <- write_site(ed_veg$site, path_prefix, ed_veg$latitude, ed_veg$longitude)
  invisible(
    list(
      css = css_file,
      pss = pss_file,
      site = site_file
    )
  )
}

#' Write individual ED inputs
#'
#' Functions for writing css, pss, and site files from their respective objects.
#'
#' Latitude and longitude coordinates will be converted directly to character, 
#' without any changes to their precision. If they are `NULL` (default), the 
#' function assumes that `lat` and `lon` are already in the `path_prefix`, and 
#' if they are absent, the function will throw an error.
#'
#' @param css css object (see [read_css])
#' @param pss pss object (see [read_pss])
#' @param site site object (see [read_site])
#' @param latitude Site latitude coordinate (default = `NULL`)
#' @param longitude Site longitude coordinate (default = `NULL`)
#' @inheritParams write_ed_veg
#' @return Full file path as character, invisibly
#' @export
write_css <- function(css, path_prefix, latitude = NULL, longitude = NULL) {
  css_fname <- prepare_ed_veg_filename(path_prefix, ".css", latitude, longitude)
  write.table(css, css_fname, quote = FALSE, row.names = FALSE)
  invisible(css_fname)
}

#' @rdname write_css
#' @export
write_pss <- function(pss, path_prefix, latitude = NULL, longitude = NULL) {
  pss_fname <- prepare_ed_veg_filename(path_prefix, ".pss", latitude, longitude)
  write.table(pss, pss_fname, quote = FALSE, row.names = FALSE)
  invisible(pss_fname)
}

#' @rdname write_css
#' @export
write_site <- function(site, path_prefix, latitude = NULL, longitude = NULL) {
  site_fname <- prepare_ed_veg_filename(path_prefix, ".site", latitude, longitude)
  first_line <- sprintf(
    "nsite %d file_format %d",
    attr(site, "nsite"),
    attr(site, "file_format")
  )
  writeLines(first_line, site_fname)
  write.table(site, site_fname, quote = FALSE, row.names = FALSE, append = TRUE)
  invisible(site_fname)
}

#' Format file name for ED vegetation inputs
#'
#' Adds the latitude and longitude, or checks if they are formatted correctly. 
#' Then, splits the prefix into the directory and base name, appends the suffix 
#' to the base name (adding a starting dot, if necessary), and returns the 
#' filename as a character.
#'
#' @param suffix Character string of filename suffix.
#' @inheritParams write_css
#' @return Character string of full formatted file path
prepare_ed_veg_filename <- function(path_prefix, suffix, latitude = NULL, longitude = NULL) {
  if (!is.null(latitude) && !is.null(longitude)) {
    path_prefix_full <- paste0(
      path_prefix, ".",
      "lat", as.character(latitude),
      "lon", as.character(longitude)
    )
  } else {
    assertthat::assert_that(grepl("lat[[:digit:]]+(\\.[[:digit:]]+)?lon[[:digit:]]+(\\.[[:digit:]]+)?", path_prefix))
    path_prefix_full <- path_prefix
  }
  base_name <- basename(path_prefix_full)
  dir_name <- dirname(path_prefix_full)
  dir.create(dir_name, showWarnings = FALSE)

  if (!grepl("^\\.", suffix)) {
    suffix <- paste0(".", suffix)
  }
  file.path(dir_name, paste0(base_name, suffix))
}
