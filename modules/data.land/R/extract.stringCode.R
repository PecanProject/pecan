#' extract.stringCode
#'
#' @param x string to decode
#' @param extractor function to apply
#'
#' @export
extract.stringCode <- function(x, extractor = from.TreeCode) {
  return(extractor(x))
} # extract.stringCode

#' from.TreeCode
#' @param x string to decode
#' @export
from.TreeCode <- function(x) {
  SITE <- substr(x, 1, 1)
  PLOT <- substr(x, 2, 2)
  SUBPLOT <- substr(x, 3, 3)
  TAG <- substr(x, 4, 1000000L)
  return(data.frame(SITE, PLOT, SUBPLOT, TAG))
} # from.TreeCode

#' to.TreeCode
#' @param SITE,PLOT,SUBPLOT,TAG strings (or coercible to)
#' @export
to.TreeCode <- function(SITE, PLOT, SUBPLOT, TAG = NULL) {
  SITE <- as.character(SITE)
  PLOT <- as.character(PLOT)
  SUBPLOT <- as.character(SUBPLOT)
  TAG <- as.character(TAG)
  x <- paste0(SITE, PLOT, SUBPLOT)
  if (!is.null(x)) {
    x <- paste0(x, TAG)
  }
  return(x)
} # to.TreeCode

#' from.Tag
#' @param x string to decode
#' @export
from.Tag <- function(x) {
  miss <- rep(NA, length(x))
  return(data.frame(SITE = miss, PLOT = miss, SUBPLOT = miss, TAG = x))
} # from.Tag

#' to.Tag
#' @param SITE,PLOT,SUBPLOT ignored
#' @param TAG string (or coercible to)
#' @export
to.Tag <- function(SITE, PLOT, SUBPLOT, TAG = NULL) {
  SITE <- as.character(SITE)
  PLOT <- as.character(PLOT)
  SUBPLOT <- as.character(SUBPLOT)
  return(as.character(TAG))
} # to.Tag
