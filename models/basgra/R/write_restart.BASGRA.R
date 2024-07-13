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
    if (new.state$LAI < 0) analysis.save[[length(analysis.save)]] <- 0.0001
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
  
  if ("soil_organic_nitrogen_content" %in% variables) {
    analysis.save[[length(analysis.save) + 1]] <- new.state$soil_nitrogen_content
    if (new.state$soil_nitrogen_content < 0) analysis.save[[length(analysis.save)]] <- 0
    names(analysis.save[[length(analysis.save)]]) <- c("soil_nitrogen_content")
  }
  
  if ("TotSoilCarb" %in% variables) {
    analysis.save[[length(analysis.save) + 1]] <- new.state$TotSoilCarb
    if (new.state$TotSoilCarb < 0) analysis.save[[length(analysis.save)]] <- 0
    names(analysis.save[[length(analysis.save)]]) <- c("TotSoilCarb")
  }
  
  if ("CropYield" %in% variables) {
     analysis.save[[length(analysis.save) + 1]] <- new.state$CropYield
     if (new.state$CropYield < 0) analysis.save[[length(analysis.save)]] <- 0
     names(analysis.save[[length(analysis.save)]]) <- c("CropYield")
  }
  
  if ("litter_carbon_content" %in% variables) {
    analysis.save[[length(analysis.save) + 1]] <- new.state$litter_carbon_content
    if (new.state$litter_carbon_content < 0) analysis.save[[length(analysis.save)]] <- 0
    names(analysis.save[[length(analysis.save)]]) <- c("litter_carbon_content")
  }
  
  if ("stubble_carbon_content" %in% variables) {
    analysis.save[[length(analysis.save) + 1]] <- new.state$stubble_carbon_content
    if (new.state$stubble_carbon_content < 0) analysis.save[[length(analysis.save)]] <- 0
    names(analysis.save[[length(analysis.save)]]) <- c("stubble_carbon_content")
  }
  
  if ("stem_carbon_content" %in% variables) {
    analysis.save[[length(analysis.save) + 1]] <- new.state$stem_carbon_content
    if (new.state$stem_carbon_content < 0) analysis.save[[length(analysis.save)]] <- 0
    names(analysis.save[[length(analysis.save)]]) <- c("stem_carbon_content")
  }
  
  if ("root_carbon_content" %in% variables) {
    analysis.save[[length(analysis.save) + 1]] <- new.state$root_carbon_content
    if (new.state$root_carbon_content < 0) analysis.save[[length(analysis.save)]] <- 0.0001
    names(analysis.save[[length(analysis.save)]]) <- c("root_carbon_content")
  }
  
  if ("reserve_carbon_content" %in% variables) {
    analysis.save[[length(analysis.save) + 1]] <- new.state$reserve_carbon_content
    if (new.state$reserve_carbon_content < 0) analysis.save[[length(analysis.save)]] <- 1e-05
    names(analysis.save[[length(analysis.save)]]) <- c("reserve_carbon_content")
  }
  
  if ("leaf_carbon_content" %in% variables) {
    analysis.save[[length(analysis.save) + 1]] <- new.state$leaf_carbon_content
    if (new.state$leaf_carbon_content < 0) analysis.save[[length(analysis.save)]] <- 0.001
    names(analysis.save[[length(analysis.save)]]) <- c("leaf_carbon_content")
  }

  if ("dead_leaf_carbon_content" %in% variables) {
    analysis.save[[length(analysis.save) + 1]] <- new.state$dead_leaf_carbon_content
    if (new.state$dead_leaf_carbon_content < 0) analysis.save[[length(analysis.save)]] <- 0
    names(analysis.save[[length(analysis.save)]]) <- c("dead_leaf_carbon_content")
  }
  
  if ("nonelongating_generative_tiller" %in% variables) {
    analysis.save[[length(analysis.save) + 1]] <- new.state$nonelongating_generative_tiller
    if (new.state$nonelongating_generative_tiller < 0) analysis.save[[length(analysis.save)]] <- 10
    names(analysis.save[[length(analysis.save)]]) <- c("nonelongating_generative_tiller")
  }
  
  if ("elongating_generative_tiller" %in% variables) {
    analysis.save[[length(analysis.save) + 1]] <- new.state$elongating_generative_tiller
    if (new.state$elongating_generative_tiller < 0) analysis.save[[length(analysis.save)]] <- 0
    names(analysis.save[[length(analysis.save)]]) <- c("elongating_generative_tiller")
  }
  
  if ("nonelongating_vegetative_tiller" %in% variables) {
    analysis.save[[length(analysis.save) + 1]] <- new.state$nonelongating_vegetative_tiller
    if (new.state$nonelongating_vegetative_tiller < 0) analysis.save[[length(analysis.save)]] <- 100
    names(analysis.save[[length(analysis.save)]]) <- c("nonelongating_vegetative_tiller")
  }
  
  if ("tiller_density" %in% variables) {
    analysis.save[[length(analysis.save) + 1]] <- new.state$tiller_density
    if (new.state$tiller_density < 0) analysis.save[[length(analysis.save)]] <- 100
    names(analysis.save[[length(analysis.save)]]) <- c("tiller_density")
  }
  
  if ("phenological_stage" %in% variables) {
    analysis.save[[length(analysis.save) + 1]] <- new.state$phenological_stage
    if (new.state$phenological_stage < 0) analysis.save[[length(analysis.save)]] <- 0
    if (new.state$phenological_stage > 1) analysis.save[[length(analysis.save)]] <- 1
    names(analysis.save[[length(analysis.save)]]) <- c("phenological_stage")
  }
  
  if ("SoilMoistFrac" %in% variables) {
    analysis.save[[length(analysis.save) + 1]] <- new.state$SoilMoistFrac
    if (new.state$SoilMoistFrac < 0) analysis.save[[length(analysis.save)]] <- 0.001
    if (new.state$SoilMoistFrac > 1) analysis.save[[length(analysis.save)]] <- 1
    names(analysis.save[[length(analysis.save)]]) <- c("SoilMoistFrac")
  }
  
  if (!is.null(analysis.save) && length(analysis.save) > 0){
    analysis.save.mat <- data.frame(matrix(unlist(analysis.save, use.names = TRUE), nrow = 1))
    colnames(analysis.save.mat) <- names(unlist(analysis.save))
  }else{
    analysis.save.mat <- NULL
  }
  
  PEcAn.logger::logger.info(runid)
  PEcAn.logger::logger.info(analysis.save.mat)
  
  settings$run$inputs$met <- inputs$met
  do.call(write.config.BASGRA, args = list(defaults     = NULL,
                                           trait.values = new.params,
                                           settings = settings,
                                           run.id = runid,
                                           IC = analysis.save.mat))
  
    
  return(TRUE)
} # write_restart.BASGRA