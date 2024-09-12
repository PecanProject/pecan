##' @title Read restart function for SDA with SIPNET
##' 
##' @author Ann Raiho \email{araiho@@nd.edu}
##' 
##' @inheritParams PEcAn.ModelName::read_restart.ModelName
##' 
##' @description Read Restart for SIPNET
##' 
##' @return X.vec      vector of forecasts
##' @export
read_restart.SIPNET <- function(outdir, runid, stop.time, settings, var.names, params) {
  
  prior.sla <- params[[which(!names(params) %in% c("soil", "soil_SDA", "restart"))[1]]]$SLA
  
  forecast <- list()
  params$restart <-c() #state.vars not in var.names will be added here
  #SIPNET inital states refer to models/sipnet/inst/template.param
  state.vars <- c("SWE", "SoilMoistFrac", "AbvGrndWood", "TotSoilCarb", "LAI", 
                  "litter_carbon_content", "fine_root_carbon_content", 
                  "coarse_root_carbon_content", "litter_mass_content_of_water")
  #when adding new state variables make sure the naming is consistent across read_restart, write_restart and write.configs
  #pre-populate parsm$restart with NAs so state names can be added
  params$restart <- rep(NA, length(setdiff(state.vars, var.names)))
  #add states to params$restart NOT in var.names
  names(params$restart) <- setdiff(state.vars, var.names)
  # Read ensemble output
  ens <- PEcAn.utils::read.output(runid = runid,
                                  outdir = file.path(outdir, runid),
                                  start.year = lubridate::year(stop.time),
                                  end.year = lubridate::year(stop.time),
                                  variables = c(state.vars,"time_bounds"))
  #calculate last
  start.time <- as.Date(paste0(lubridate::year(stop.time),"-01-01"))
  time_var <- ens$time_bounds[1,]
  real_time <- as.POSIXct(time_var*3600*24, origin = start.time)
  # last <- which(as.Date(real_time)==as.Date(stop.time))[1]
  last <- which(as.Date(real_time)==as.Date(stop.time))[length(which(as.Date(real_time)==as.Date(stop.time)))]
  
  #### PEcAn Standard Outputs
  if ("AbvGrndWood" %in% var.names) {
    forecast[[length(forecast) + 1]] <- PEcAn.utils::ud_convert(ens$AbvGrndWood[last],  "kg/m^2", "Mg/ha")
    names(forecast[[length(forecast)]]) <- c("AbvGrndWood")
    
    wood_total_C    <- ens$AbvGrndWood[last] + ens$fine_root_carbon_content[last] + ens$coarse_root_carbon_content[last]
    if (wood_total_C<=0) wood_total_C <- 0.0001 # Making sure we are not making Nans in case there is no plant living there.
    
    params$restart["abvGrndWoodFrac"] <- ens$AbvGrndWood[last]  / wood_total_C
    params$restart["coarseRootFrac"]  <- ens$coarse_root_carbon_content[last] / wood_total_C
    params$restart["fineRootFrac"]    <- ens$fine_root_carbon_content[last]   / wood_total_C
  }else{
    params$restart["AbvGrndWood"] <- PEcAn.utils::ud_convert(ens$AbvGrndWood[last],  "kg/m^2", "g/m^2")
    # calculate fractions, store in params, will use in write_restart
    wood_total_C    <- ens$AbvGrndWood[last] + ens$fine_root_carbon_content[last] + ens$coarse_root_carbon_content[last]
    if (wood_total_C<=0) wood_total_C <- 0.0001 # Making sure we are not making Nans in case there is no plant living there.
    
    params$restart["abvGrndWoodFrac"] <- ens$AbvGrndWood[last]  / wood_total_C
    params$restart["coarseRootFrac"]  <- ens$coarse_root_carbon_content[last] / wood_total_C
    params$restart["fineRootFrac"]    <- ens$fine_root_carbon_content[last]   / wood_total_C
  }
  
  if ("GWBI" %in% var.names) {
    forecast[[length(forecast) + 1]] <- PEcAn.utils::ud_convert(mean(ens$GWBI),  "kg/m^2/s", "Mg/ha/yr")
    names(forecast[[length(forecast)]]) <- c("GWBI")
  }
  
  # Reading in NET Ecosystem Exchange for SDA - unit is kg C m-2 s-1 and the average is estimated
  if ("NEE" %in% var.names) {
    forecast[[length(forecast) + 1]] <- mean(ens$NEE)  ## 
    names(forecast[[length(forecast)]]) <- c("NEE")
  }
  
  
  # Reading in Latent heat flux for SDA  - unit is MW m-2
  if ("Qle" %in% var.names) {
    forecast[[length(forecast) + 1]] <- ens$Qle[last]*1e-6  ##  
    names(forecast[[length(forecast)]]) <- c("Qle")
  }
  
  if ("leaf_carbon_content" %in% var.names) {
    forecast[[length(forecast) + 1]] <- ens$leaf_carbon_content[last]  ## kgC/m2*m2/kg*2kg/kgC
    names(forecast[[length(forecast)]]) <- c("LeafC")
  }
  
  if ("LAI" %in% var.names) {
    forecast[[length(forecast) + 1]] <- ens$LAI[last]  ## m2/m2 
    names(forecast[[length(forecast)]]) <- c("LAI")
  }else{
    params$restart["LAI"] <- ens$LAI[last]
  }
  
  if ("litter_carbon_content" %in% var.names) {
    forecast[[length(forecast) + 1]] <- ens$litter_carbon_content[last]  ##kgC/m2
    names(forecast[[length(forecast)]]) <- c("litter_carbon_content")
  }else{
    params$restart["litter_carbon_content"] <- PEcAn.utils::ud_convert(ens$litter_carbon_content[last], 'kg m-2', 'g m-2') # kgC/m2 -> gC/m2
  }
  
  if ("litter_mass_content_of_water" %in% var.names) {
    forecast[[length(forecast) + 1]] <- ens$litter_mass_content_of_water[last]  ##kgC/m2
    names(forecast[[length(forecast)]]) <- c("litter_mass_content_of_water")
  }else{
    params$restart["litter_mass_content_of_water"] <- ens$litter_mass_content_of_water[last]
  }
  
  if ("SoilMoistFrac" %in% var.names) {
    forecast[[length(forecast) + 1]] <- ens$SoilMoistFrac[last]*100  ## here we multiply it by 100 to convert from proportion to percentage.
    names(forecast[[length(forecast)]]) <- c("SoilMoistFrac")
  }else{
    params$restart["SoilMoistFrac"] <- ens$SoilMoistFrac[last]
  }
  
  # This is snow
  if ("SWE" %in% var.names) {
    forecast[[length(forecast) + 1]] <- ens$SWE[last]  ## kgC/m2
    names(forecast[[length(forecast)]]) <- c("SWE")
  }else{
    params$restart["SWE"] <- ens$SWE[last]/10
  }
  
  if ("TotLivBiom" %in% var.names) {
    forecast[[length(forecast) + 1]] <- PEcAn.utils::ud_convert(ens$TotLivBiom[last],  "kg/m^2", "Mg/ha")
    names(forecast[[length(forecast)]]) <- c("TotLivBiom")
  }
  
  if ("TotSoilCarb" %in% var.names) {
    forecast[[length(forecast) + 1]] <- ens$TotSoilCarb[last]
    names(forecast[[length(forecast)]]) <- c("TotSoilCarb")
  }else{
    params$restart["TotSoilCarb"] <- PEcAn.utils::ud_convert(ens$TotSoilCarb[last], 'kg m-2', 'g m-2') # kgC/m2 -> gC/m2
  }
  
  #remove any remaining NAs from params$restart
  params$restart <- stats::na.omit(params$restart)
  
  print(runid)
  
  X_tmp <- list(X = unlist(forecast), params = params)
  
  return(X_tmp)
} # read_restart.SIPNET