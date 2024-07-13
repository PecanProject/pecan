
#' Write SIBCASA met files
#'
#' Converts a met CF file to a sibcasa specific met file. The input
#' files are calld <in.path>/<in.prefix>.YYYY.cf
#'
#' @param in.path path on disk where CF file lives
#' @param in.prefix prefix for each file
#' @param outfolder location where model specific output is written
#' @param overwrite logical: Replace output if it already exists?
#' @return OK if everything was succesful.
#' @export
#' @author Tony Gardella
#'
met2model.SIBCASA <- function(in.path, in.prefix, outfolder, overwrite = FALSE) {
  PEcAn.logger::logger.severe("NOT IMPLEMENTED")

  # Please follow the PEcAn style guide:
  # https://pecanproject.github.io/pecan-documentation/master/coding-style.html

  # Note that `library()` calls should _never_ appear here; instead, put
  # packages dependencies in the DESCRIPTION file, under "Imports:".
  # Calls to dependent packages should use a double colon, e.g.
  #    `packageName::functionName()`.
  # Also, `require()` should be used only when a package dependency is truly
  # optional. In this case, put the package name under "Suggests:" in DESCRIPTION.
}
