#' Check individual ED input files
#'
#' Check internal file formatting, and optionally check for compatibility 
#' against related files.
#'
#' @param css css data object (see [read_css])
#' @param pss pss data object (see [read_pss])
#' @param site site data object (see [read_site])
#' @return `NULL` (invisibly)
#' @export
check_css <- function(css, pss = NULL) {
  if(!inherits(css, "data.frame") || nrow(css) == 0) {
    stop("css file should be a data frame")
  }
  
  expected_colnames <- c("time", "patch", "cohort", "dbh", "hite", "pft",
    "n", "bdead", "balive", "lai")
  if (!identical(colnames(css), expected_colnames)) {
    stop("css file is formatted incorrectly")
  }
  
  if (!is.null(pss)) {
    if(!all(unique(css$patch) %in% unique(pss$patch))) {
      stop("css file and pss file are not compatible")
    }
  }
  
}

#' @rdname check_css
#' @export
check_pss <- function(pss, site = NULL) {
  if (!inherits(pss, "data.frame") || nrow(pss) == 0) {
    stop("css file should be a data frame")
  }
  if (!is.null(site)) {
    
    if(!all(unique(pss$site) %in% unique(site$sitenum))) {
      stop("pss and site files are not compatible")
    }
  }
}

#' @rdname check_css
#' @export
check_site <- function(site) {
      stopifnot(
        nrow(site) >= 1,
        !is.null(attributes(site)),
        is.numeric(attr(site, "nsite")),
        attr(site, "file_format") %in% c(1, 2, 3))
}
