#' @title Write restart template for SDA
#' 
#' @author Alexey Shiklomanov
#' 
#' @param outdir
#' @param runid
#' @param start.time   Time of current assimilation step 
#' @param stop.time    Time of next assimilation step
#' @param settings
#' @param new.state    Analysis state matrix returned by \code{sda.enkf}
#' @param RENAME       flag to either rename output file or not
#' @param new.params   optional, additionals params to pass write.configs that are deterministically related to the parameters updated by the analysis 
#' @param inputs       new input paths updated by the SDA workflow, will be passed to write.configs
#' 
#' @description Write restart files for model
#' 
#' @export
write_restart.ModelName <- function(outdir,
                                    runid,
                                    start.time,
                                    stop.time,
                                    settings,
                                    new.state,
                                    RENAME,
                                    new.params,
                                    inputs) {}
