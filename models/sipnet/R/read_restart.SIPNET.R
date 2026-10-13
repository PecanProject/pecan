##' @title Read restart function for SDA with SIPNET
##' 
##' @author Ann Raiho \email{araiho@@nd.edu}
##' 
##' @inheritParams PEcAn.ModelName::read_restart.ModelName
##' 
##' @param start.time Start of the forecast interval. If NULL, use January 1
##'   of the year containing stop.time.
##' @description Read Restart for SIPNET
##' 
##' @return X.vec      vector of forecasts
##' @export
read_restart.SIPNET <- function(outdir, runid, stop.time, settings, var.names, params, start.time = NULL) {
  
  prior.sla <- params[[which(!names(params) %in% c("soil", "soil_SDA", "restart"))[1]]]$SLA
  
  forecast <- list()
  params$restart <-c() #state.vars not in var.names will be added here
  #SIPNET inital states refer to models/sipnet/inst/template.param
  state.vars <- c(
    "SWE",
    "SoilMoist",
    "SoilMoistFrac",
    "AbvGrndWood",
    "NEE",
    "Qle",
    "TotSoilCarb",
    "LAI",
    "litter_carbon_content",
    "fine_root_carbon_content",
    "coarse_root_carbon_content",
    "litter_mass_content_of_water"
  )
  #when adding new state variables make sure the naming is consistent across read_restart, write_restart and write.configs
  #pre-populate parsm$restart with NAs so state names can be added
  params$restart <- rep(NA, length(setdiff(state.vars, var.names)))
  #add states to params$restart NOT in var.names
  names(params$restart) <- setdiff(state.vars, var.names)
  # Read the current forecast interval from annual NetCDF files.
  if (is.null(start.time)) {
    start.time <- as.POSIXct(
      paste0(lubridate::year(stop.time), "-01-01"), tz = "UTC"
    )
  }
  # Read ensemble output
  ens <- PEcAn.utils::read.output(
    runid = runid,
    outdir = file.path(outdir, runid),
    start.year = lubridate::year(start.time),
    end.year = lubridate::year(stop.time),
    variables = unique(c(state.vars, var.names)),
    dataframe = TRUE
  )
  ens <- ens[
    ens$posix >= start.time & ens$posix <= stop.time,
    , drop = FALSE
  ]
  if (!nrow(ens)) {
    stop("No SIPNET output in the forecast interval.", call. = FALSE)
  }
  last <- nrow(ens)
  
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
  
  # Reading in NET Ecosystem Exchange for SDA - kg C m-2 s-1 -> g C m-2 day-1 and the average is estimated
  if ("NEE" %in% var.names) {
    forecast[[length(forecast) + 1]] <- nee_model_to_obs(
      get_interval_mean(ens, "NEE")
    )
    names(forecast[[length(forecast)]]) <- "NEE"
  }
  
  # Reading in Latent heat flux for SDA  - unit is W m-2 and the average is estimated
  if ("Qle" %in% var.names) {
    forecast[[length(forecast) + 1]] <- get_interval_mean(ens, "Qle")
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
  
  litter_carbon_content <- ens$litter_carbon_content[last] %||% NA_real_  ##kgC/m2
  if ("litter_carbon_content" %in% var.names) {
    forecast[[length(forecast) + 1]] <- litter_carbon_content
    names(forecast[[length(forecast)]]) <- c("litter_carbon_content")
  }else{
    params$restart["litter_carbon_content"] <- PEcAn.utils::ud_convert(litter_carbon_content, 'kg m-2', 'g m-2')
  }
  
  litter_mass_content_of_water <- ens$litter_mass_content_of_water[last] %||% NA_real_  ##kgC/m2
  if ("litter_mass_content_of_water" %in% var.names) {
    forecast[[length(forecast) + 1]] <- litter_mass_content_of_water
    names(forecast[[length(forecast)]]) <- c("litter_mass_content_of_water")
  }else{
    params$restart["litter_mass_content_of_water"] <- litter_mass_content_of_water
  }
  
  if ("SoilMoist" %in% var.names) {
    forecast[[length(forecast) + 1]] <- ens$SoilMoist[last]
    names(forecast[[length(forecast)]]) <- c("SoilMoist")
  }else{
    params$restart["SoilMoist"] <- ens$SoilMoist[last]
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
  
  X_tmp <- list(X = unlist(forecast), params = params)
  
  return(X_tmp)
} # read_restart.SIPNET


##' @title Calculate the mean of a model output variable
##'
##' @description Calculates the mean of a variable over the model output
##' supplied in \code{ens}, excluding missing values.
##'
##' @param ens List of model output variables.
##' @param v Character. Name of the variable to average.
##'
##' @details The averaging interval is determined by the data supplied in
##' \code{ens}. This function does not filter timestamps.
##'
##' @return Numeric scalar containing the mean of the non-missing values.
##' @keywords internal
##' @noRd
get_interval_mean <- function(ens, v) {
  x <- ens[[v]]
  if (is.null(x)) {
    stop("Variable `", v, "` is missing from NetCDF.", call. = FALSE)
  }
  
  x <- as.numeric(x)
  if (!length(x) || all(is.na(x))) {
    stop("Variable `", v, "` has no valid values.", call. = FALSE)
  }
  
  mean(x, na.rm = TRUE)
}


##' @title Convert model NEE to SDA observation units
##'
##' @description Converts model NEE from kg C m-2 s-1 to
##' g C m-2 day-1 for comparison with SDA observations.
##'
##' @param x Numeric vector of NEE values in kg C m-2 s-1.
##'
##' @details The conversion factor is approximately \code{1000 * 86400}.
##' The sign of NEE is preserved. Corresponding SDA observations must
##' use g C m-2 day-1.
##'
##' @return Numeric vector of NEE values in g C m-2 day-1.
##' @keywords internal
##' @noRd
nee_model_to_obs <- function(x) {
  x * 1e8 / 1.157407
}