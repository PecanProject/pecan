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
