#' Create full ED vegetation input object
#'
#' @param css [css][read_css] object (`data.frame`)
#' @param pss [pss][read_pss] object (`data.frame`)
#' @param site [site][read_site] object (`data.frame`)
#' @param latitude Latitude coordinate
#' @param longitude Longitude coordinate
#' @param ... Additional objects to store in list
#' @inheritParams create_css
#' @export
create_ed_veg <- function(css, pss, site, latitude, longitude, check = TRUE, ...) {
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
    ...
  )
}

#' Create css, pss, and site files from examples
#'
#' @param input Named `list` or `data.frame` containing columns to replace in examples
#' @param check Logical. If `TRUE` (default), also check files for validity.
#' @return css, pss, or site object (`data.frame`, possibly with attributes)
#' @export
create_css <- function(input, check = TRUE) {
  new_css <- modify_df(input, example_css)
  if (check) {
    check_css(new_css)
  }
  new_css
}

#' @rdname create_css
#' @export
create_pss <- function(input, check = TRUE) {
  new_pss <- modify_df(input, example_pss)
  if (check) {
    check_pss(new_pss)
  }
  new_pss
}

#' @rdname create_css
#' @export
create_site <- function(input, check = TRUE) {
  new_site <- modify_df(input, example_site)
  attr(new_site, "nsite") <- nrow(new_site)
  attr(new_site, "file_format") <- 1
  if (check) {
    check_site(new_site)
  }
  new_site
}

#' Modify a reference `data.frame`
#' 
#' Wrapper around `modifyList` to allow expanding a `data.frame` by modifying 
#' only a single column.
#'
#' @param input Named `list` or `data.frame` containing columns to replace in `base`
#' @param base Reference object to modify
#' @return Modified `data.frame`
modify_df <- function(input, base) {
  col_names <- colnames(base)
  out_list <- modifyList(as.list(base), as.list(input))
  as.data.frame(out_list)[col_names]
}
