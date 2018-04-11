#' Read restart template for SDA
#' 
#' @author Alexey Shiklomanov
#' 
#' @param outdir      Output directory
#' @param runid       Run ID
#' @param stop.time   Year that is being read
#' @param settings    PEcAn settings object
#' @param var.names   Variable names to be extracted
#' @param params      Any parameters required for state calculations
#' 
#' @description Read restart files from model.
#' 
#' @return Forecast numeric matrix
#' @export
read_restart.SIBCASA<- function(outdir,
                                   runid, 
                                   stop.time,
                                   settings,
                                   var.names,
                                   params) {}

