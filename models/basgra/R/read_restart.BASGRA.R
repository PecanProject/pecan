##' @title Read restart function for SDA with BASGRA
##' 
##' @author Istem Fer
##' 
##' @inheritParams PEcAn.ModelName::read_restart.ModelName
##' 
##' @description Read Restart for BASGRA
##' 
##' @return X.vec      vector of forecasts
##' @export
##' 
read_restart.BASGRA <- function(outdir, runid, stop.time, settings, var.names, params) {
  
  forecast <- list()

  # maybe have some checks here to make sure the first run is actually ran for the period you requested
    
  # Read ensemble output
  ens <- PEcAn.utils::read.output(runid = runid,
                                  outdir = file.path(outdir, runid), 
                                  start.year = lubridate::year(stop.time), 
                                  end.year = lubridate::year(stop.time),
                                  variables = var.names)
  
  last <- length(ens[[1]])
  
  params$restart <- c()
  
  if ("LAI" %in% var.names) {
    forecast[[length(forecast) + 1]] <- ens$LAI[last] ## m2 m-2 
    names(forecast[[length(forecast)]]) <- c("LAI")
  }
  
  if ("fast_soil_pool_carbon_content" %in% var.names) {
    forecast[[length(forecast) + 1]] <- ens$fast_soil_pool_carbon_content[last] # kg C m-2
    names(forecast[[length(forecast)]]) <- c("fast_soil_pool_carbon_content")
  }
  
  if ("slow_soil_pool_carbon_content" %in% var.names) {
    forecast[[length(forecast) + 1]] <- ens$slow_soil_pool_carbon_content[last] # kg C m-2
    names(forecast[[length(forecast)]]) <- c("slow_soil_pool_carbon_content")
  }
  
  if ("soil_organic_nitrogen_content" %in% var.names) {
    forecast[[length(forecast) + 1]] <- ens$soil_nitrogen_content[last]  # kg N m-2
    names(forecast[[length(forecast)]]) <- c("soil_nitrogen_content")
  }
  
  if ("TotSoilCarb" %in% var.names) {
    forecast[[length(forecast) + 1]] <- ens$TotSoilCarb[last]  # kg C m-2
    names(forecast[[length(forecast)]]) <- c("TotSoilCarb")
  }
  
  if ("NEE" %in% var.names) {
    forecast[[length(forecast) + 1]] <- ens$NEE[last]  # kg C m-2 s-1
    names(forecast[[length(forecast)]]) <- c("NEE")
  }
  
  if ("NPP" %in% var.names) {
    forecast[[length(forecast) + 1]] <- ens$NPP[last]  # kg C m-2 s-1
    names(forecast[[length(forecast)]]) <- c("NPP")
  }
  
  if ("Qle" %in% var.names) {
    forecast[[length(forecast) + 1]] <- ens$Qle[last]  # W m-2
    names(forecast[[length(forecast)]]) <- c("Qle")
  }
  
  if ("SoilResp" %in% var.names) {
    forecast[[length(forecast) + 1]] <- ens$SoilResp[last]  # kg C m-2 s-1
    names(forecast[[length(forecast)]]) <- c("SoilResp")
  }
  
  if ("CropYield" %in% var.names) {
   forecast[[length(forecast) + 1]] <- ens$CropYield[last]  # kg DM m-2 TODO: check PEcAn standard units if DM is OK
   names(forecast[[length(forecast)]]) <- c("CropYield")
  }
  
  if ("litter_carbon_content" %in% var.names) {
    forecast[[length(forecast) + 1]] <- ens$litter_carbon_content[last]  # kg C m-2
    names(forecast[[length(forecast)]]) <- c("litter_carbon_content")
  }
  
  if ("stubble_carbon_content" %in% var.names) {
    forecast[[length(forecast) + 1]] <- ens$stubble_carbon_content[last]  # kg C m-2
    names(forecast[[length(forecast)]]) <- c("stubble_carbon_content")
  }
  
  if ("stem_carbon_content" %in% var.names) {
    forecast[[length(forecast) + 1]] <- ens$stem_carbon_content[last]  # kg C m-2
    names(forecast[[length(forecast)]]) <- c("stem_carbon_content")
  }
  
  if ("root_carbon_content" %in% var.names) {
    forecast[[length(forecast) + 1]] <- ens$root_carbon_content[last]  # kg C m-2
    names(forecast[[length(forecast)]]) <- c("root_carbon_content")
  }
  
  if ("reserve_carbon_content" %in% var.names) {
    forecast[[length(forecast) + 1]] <- ens$reserve_carbon_content[last]  # kg C m-2
    names(forecast[[length(forecast)]]) <- c("reserve_carbon_content")
  }
  
  if ("leaf_carbon_content" %in% var.names) {
    forecast[[length(forecast) + 1]] <- ens$leaf_carbon_content[last]  # kg C m-2
    names(forecast[[length(forecast)]]) <- c("leaf_carbon_content")
  }
  
  if ("dead_leaf_carbon_content" %in% var.names) {
    forecast[[length(forecast) + 1]] <- ens$dead_leaf_carbon_content[last]  # kg C m-2
    names(forecast[[length(forecast)]]) <- c("dead_leaf_carbon_content")
  }
  
  # I'm not deleting the following but updating the overall tiller_density in SDA worked better, so use it instead in the SDA.xml
  if ("nonelongating_generative_tiller" %in% var.names) {
    forecast[[length(forecast) + 1]] <- ens$nonelongating_generative_tiller[last]  # m-2
    names(forecast[[length(forecast)]]) <- c("nonelongating_generative_tiller")
  }
  
  if ("elongating_generative_tiller" %in% var.names) {
    forecast[[length(forecast) + 1]] <- ens$elongating_generative_tiller[last]  # m-2
    names(forecast[[length(forecast)]]) <- c("elongating_generative_tiller")
  }
  
  if ("nonelongating_vegetative_tiller" %in% var.names) {
    forecast[[length(forecast) + 1]] <- ens$nonelongating_vegetative_tiller[last]  # m-2
    names(forecast[[length(forecast)]]) <- c("nonelongating_vegetative_tiller")
  } 
  
  if ("tiller_density" %in% var.names) {
    forecast[[length(forecast) + 1]] <- ens$tiller_density[last]
    names(forecast[[length(forecast)]]) <- c("tiller_density")
  }
  
  if ("phenological_stage" %in% var.names) {
    forecast[[length(forecast) + 1]] <- ens$phenological_stage[last]  
    names(forecast[[length(forecast)]]) <- c("phenological_stage")
  } 
  
  if ("SoilMoistFrac" %in% var.names) {
    forecast[[length(forecast) + 1]] <- ens$SoilMoistFrac[last]  
    names(forecast[[length(forecast)]]) <- c("SoilMoistFrac")
  } 

  if ("harvest_carbon_flux" %in% var.names) {
    forecast[[length(forecast) + 1]] <- ens$harvest_carbon_flux[last]  # kg C m-2 s-1
    names(forecast[[length(forecast)]]) <- c("harvest_carbon_flux")
  }
  
  PEcAn.logger::logger.info(runid)
  
  X_tmp <- list(X = unlist(forecast), params = params)
  
  return(X_tmp)
  
} # read_restart.BASGRA
