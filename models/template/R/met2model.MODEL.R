##' Converts a met CF file to a model specific met file. The input
##' files are calld <in.path>/<in.prefix>.YYYY.cf
##'
##' @name met2model.MODEL
##' @title Write MODEL met files
##' @param in.path path on disk where CF file lives
##' @param in.prefix prefix for each file
##' @param outfolder location where model specific output is written.
##' @param overwrite logical: replace output files if they already exist?
##' @return OK if everything was succesful.
##' @export
##' @author Rob Kooper
##-------------------------------------------------------------------------------------------------#
met2model.MODEL <- function(in.path, in.prefix, outfolder, overwrite = FALSE) {
  PEcAn.logger::logger.severe("NOT IMPLEMENTED")

  # Please follow the PEcAn style guide:
  # https://pecanproject.github.io/pecan-documentation/master/coding-style.html
  
  # Note that `library()` calls should _never_ appear here; instead, put
  # packages dependencies in the DESCRIPTION file, under "Imports:".
  # Calls to dependent packages should use a double colon, e.g.
  #    `packageName::functionName()`.
  # Also, `require()` should be used only when a package dependency is truly
  # optional. In this case, put the package name under "Suggests:" in DESCRIPTION. 
  
} # met2model.MODEL
