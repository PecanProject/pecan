##' @title write_restart.SIPNET
##' 
##' @author Istem Fer
##'
##' @inheritParams PEcAn.ModelName::write_restart.ModelName
##'
##' @description Write restart files for BASGRA 
##' 
##' @return TRUE if successful
##' @export
write_restart.BASGRA <- function(outdir, runid, start.time, stop.time, settings, new.state,
                                 RENAME = TRUE, new.params = FALSE, inputs) {
  
  return(TRUE)
} # write_restart.BASGRA