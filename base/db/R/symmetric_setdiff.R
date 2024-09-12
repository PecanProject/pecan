#' Symmetric set difference of two data frames
#'
#' @param x,y `data.frame`s to compare
#' @param xname Label for data in x but not y. Default = "x"
#' @param yname Label for data in y but not x. Default = "y"
#' @param namecol Name of label column. Default = "source".
#' @param simplify_types (Logical) If `TRUE`, coerce anything that
#'   isn't numeric to character, to facilitate comparison.
#' @return `data.frame` of data not common to x and y, with additional
#'   column (`namecol`) indicating whether data are only in x
#'   (`xname`) or y (`yname`)
#' @export
#' @examples
#' xdf <- data.frame(a = c("a", "b", "c"),
#'                   b = c(1, 2, 3),
#'                   stringsAsFactors = FALSE)
#' ydf <- data.frame(a = c("a", "b", "d"),
#'                   b = c(1, 2.5, 3),
#'                   stringsAsFactors = FALSE)
#' symmetric_setdiff(xdf, ydf)
symmetric_setdiff <- function(x, y, xname = "x", yname = "y",
                              namecol = "source", simplify_types = TRUE) {
  stopifnot(is.data.frame(x), is.data.frame(y),
            is.character(xname), is.character(yname),
            length(xname) == 1, length(yname) == 1)
  is_i64 <- c(
    vapply(x, inherits, logical(1), what = "integer64"),
    vapply(y, inherits, logical(1), what = "integer64")
  )
  if (any(is_i64)) {
    PEcAn.logger::logger.debug(
      "Detected at least one `integer64` column. ",
      "Converting to `numeric` for comparison."
    )
    if (requireNamespace("bit64", quietly = TRUE)) {
      x <- dplyr::mutate_if(x, bit64::is.integer64, as.numeric)
      y <- dplyr::mutate_if(y, bit64::is.integer64, as.numeric)
    } else {
      PEcAn.logger::logger.warn(
        '"bit64" package required for `integer64` conversion, but not installed. ',
        "Skipping conversion, which may produce weird results!"
      )
    }
  }
  if (simplify_types) {
    x <- dplyr::mutate_if(x, ~!is.numeric(.), as.character)
    y <- dplyr::mutate_if(y, ~!is.numeric(.), as.character)
  }
  namecol <- dplyr::sym(namecol)
  xy <- dplyr::setdiff(x, y) %>%
    dplyr::mutate(!!namecol := xname)
  yx <- dplyr::setdiff(y, x) %>%
    dplyr::mutate(!!namecol := yname)
  dplyr::bind_rows(xy, yx) %>%
    dplyr::select(!!namecol, dplyr::everything())
}
