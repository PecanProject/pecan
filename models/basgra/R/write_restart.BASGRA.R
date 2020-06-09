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
  
  
  rundir    <- settings$host$rundir
  variables <- colnames(new.state)
  
  settings$run$start.date <- start.time
  settings$run$end.date   <- stop.time
  
  analysis.save <- list()
  
  if ("LAI" %in% variables) {
    analysis.save[[length(analysis.save) + 1]] <- new.state$LAI  
    if (new.state$LAI < 0) analysis.save[[length(analysis.save)]] <- 0
    names(analysis.save[[length(analysis.save)]]) <- c("LAI")
  }
  
  if ("fast_soil_pool_carbon_content" %in% variables) {
    analysis.save[[length(analysis.save) + 1]] <- new.state$fast_soil_pool_carbon_content
    if (new.state$fast_soil_pool_carbon_content < 0) analysis.save[[length(analysis.save)]] <- 0
    names(analysis.save[[length(analysis.save)]]) <- c("fast_soil_pool_carbon_content")
  }
  
  if ("slow_soil_pool_carbon_content" %in% variables) {
    analysis.save[[length(analysis.save) + 1]] <- new.state$slow_soil_pool_carbon_content
    if (new.state$slow_soil_pool_carbon_content < 0) analysis.save[[length(analysis.save)]] <- 0
    names(analysis.save[[length(analysis.save)]]) <- c("slow_soil_pool_carbon_content")
  }
  
  if (!is.null(analysis.save) && length(analysis.save) > 0){
    analysis.save.mat <- data.frame(matrix(unlist(analysis.save, use.names = TRUE), nrow = 1))
    colnames(analysis.save.mat) <- names(unlist(analysis.save))
  }else{
    analysis.save.mat <- NULL
  }
  
  PEcAn.logger::logger.info(runid)
  PEcAn.logger::logger.info(analysis.save.mat)
  
  do.call(write.config.BASGRA, args = list(defaults     = NULL,
                                           trait.values = new.params,
                                           settings = settings,
                                           run.id = runid,
                                           IC = analysis.save.mat))
  
    
  return(TRUE)
} # write_restart.BASGRA