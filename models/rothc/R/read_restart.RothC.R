#' Read restart template for SDA
#'
#' Reads restart files from RothC. Not implemented yet.
#'
#' @author Chris Black
#'
#' @param outdir      Output directory
#' @param runid       Run ID
#' @param stop.time   Year that is being read
#' @param settings    PEcAn settings object
#' @param var.names   Variable names to be extracted
#' @param params      Any parameters required for state calculations
#'
#' @return Forecast numeric matrix
#' @export
read_restart.RothC <- function(outdir,
                               runid,
                               stop.time,
                               settings,
                               var.names,
                               params) {}
