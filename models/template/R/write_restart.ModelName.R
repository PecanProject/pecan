#' @title Write restart template for SDA
#' 
#' @author Alexey Shiklomanov
#' 
#' @param start.time   Time of current assimilation step 
#' @param stop.time    Time of next assimilation step
#' @param new.state    Analysis state matrix returned by \code{sda.enkf}
#' @inheritParams read.restart.ModelName
#' 
#' @description Write restart files for model
#' 
#' @export
write_restart.ModelName <- function(outdir,
                                    runid,
                                    start.time,
                                    stop.time,
                                    settings,
                                    new.state) {}
