##' Convert MODEL output into the NACP Intercomparison format (ALMA using netCDF)
##' 
##' @name model2netcdf.MODEL
##' @title Code to convert MODELS's output into netCDF format
##'
##' @param outdir Location of model output
##' @param sitelat Latitude of the site
##' @param sitelon Longitude of the site
##' @param start_date Start time of the simulation
##' @param end_date End time of the simulation
##' @export
##'
##' @author Rob Kooper
model2netcdf.MODEL <- function(outdir, sitelat, sitelon, start_date, end_date) {
  PEcAn.logger::logger.severe("NOT IMPLEMENTED")

  # Please follow the PEcAn style guide:
  # https://pecanproject.github.io/pecan-documentation/develop/coding-style.html
  
  # Note that `library()` calls should _never_ appear here; instead, put
  # packages dependencies in the DESCRIPTION file, under "Imports:".
  # Calls to dependent packages should use a double colon, e.g.
  #    `packageName::functionName()`.
  # Also, `require()` should be used only when a package dependency is truly
  # optional. In this case, put the package name under "Suggests:" in DESCRIPTION. 
  
} # model2netcdf.MODEL
