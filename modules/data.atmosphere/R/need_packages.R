#' Check if required packages are installed, and throw an informative
#' error if not.
#'
#' @param ... Package names, as characters. Can be passed as
#'   individual arguments, character vectors, or any combination thereof.
#' @return `pkgs`, invisibly
#' @author Alexey Shiklomanov
#' @examples
#' PEcAn.data.atmosphere:::need_packages("stats", "methods") # Always works 
#' try(PEcAn.data.atmosphere:::need_packages("notapackage"))
need_packages <- function(...) {
  pkgs <- unlist(list(...), recursive = TRUE)
  have <- vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)
  if (any(!have)) {
    msg <- sprintf(
      "The following packages are required but not installed: %s",
      paste0("`", pkgs, "`", collapse = ", ")
    )
    PEcAn.logger::logger.severe(msg)
  }
  invisible(pkgs)
}
