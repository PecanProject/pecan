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
read_restart.SIPNET <- function (outdir, runid, stop.time, settings, var.names, params) 
{
  prior.sla <- params[[which(!names(params) %in% c("soil", 
                                                   "soil_SDA", "restart"))[1]]]$SLA
  forecast <- list()
  params$restart <- c()
  state.vars <- c("SWE", "SoilMoist", "SoilMoistFrac", "AbvGrndWood", "NEE","Qle",
                  "TotSoilCarb", "LAI", "litter_carbon_content", "fine_root_carbon_content", 
                  "coarse_root_carbon_content", "litter_mass_content_of_water")
  
  params$restart <- rep(NA, length(setdiff(state.vars, var.names)))
  names(params$restart) <- setdiff(state.vars, var.names)
  ens <- PEcAn.utils::read.output(runid = runid, outdir = file.path(outdir, 
                                                                    runid), start.year = lubridate::year(stop.time), end.year = lubridate::year(stop.time), 
                                  variables = c(state.vars, "time_bounds"))
  
  # 4) 若关键变量缺失，直接给出可读的报错信息（避免在 ud_convert 里才崩）
  must_have <- c("AbvGrndWood","fine_root_carbon_content","coarse_root_carbon_content")
  missing <- must_have[ sapply(ens[must_have], is.null) ]
  if (length(missing) > 0) {
    stop("Missing variables in NetCDF for runid ", runid, ": ",
         paste(missing, collapse=", "),
         " | Available: ", paste(names(ens), collapse=", "),
         call. = FALSE)
  }
  
  start.time <- as.Date(paste0(lubridate::year(stop.time), 
                               "-01-01"))
  time_var <- ens$time_bounds[1, ]
  real_time <- as.POSIXct(time_var * 3600 * 24, origin = start.time)
  last <- which(as.Date(real_time) == as.Date(stop.time))[length(which(as.Date(real_time) == 
                                                                         as.Date(stop.time)))]
  
  # print("breakpoint3")
  # print(list(
  #   last = last,
  #   has_AbvGrndWood = !is.null(ens$AbvGrndWood),
  #   len_AbvGrndWood = if (is.null(ens$AbvGrndWood)) NA_integer_ else length(ens$AbvGrndWood),
  #   has_fine = !is.null(ens$fine_root_carbon_content),
  #   len_fine = if (is.null(ens$fine_root_carbon_content)) NA_integer_ else length(ens$fine_root_carbon_content),
  #   has_coarse = !is.null(ens$coarse_root_carbon_content),
  #   len_coarse = if (is.null(ens$coarse_root_carbon_content)) NA_integer_ else length(ens$coarse_root_carbon_content)
  # ))
  
  if (is.null(ens$AbvGrndWood)) {
    stop("ens$AbvGrndWood is NULL. Available vars: ", paste(names(ens), collapse=", "), call. = FALSE)
  }
  if (is.na(last) || length(ens$AbvGrndWood) < last) {
    stop("Index 'last' invalid for AbvGrndWood: last=", last,
         " len=", length(ens$AbvGrndWood), call. = FALSE)
  }
  
  if ("AbvGrndWood" %in% var.names) {
    # print("breakpoint3.1")
    forecast[[length(forecast) + 1]] <- PEcAn.utils::ud_convert(ens$AbvGrndWood[last], 
                                                                "kg/m^2", "Mg/ha")
    names(forecast[[length(forecast)]]) <- c("AbvGrndWood")
    wood_total_C <- ens$AbvGrndWood[last] + ens$fine_root_carbon_content[last] + 
      ens$coarse_root_carbon_content[last]
    if (wood_total_C <= 0) 
      wood_total_C <- 1e-04
    params$restart["abvGrndWoodFrac"] <- ens$AbvGrndWood[last]/wood_total_C
    params$restart["coarseRootFrac"] <- ens$coarse_root_carbon_content[last]/wood_total_C
    params$restart["fineRootFrac"] <- ens$fine_root_carbon_content[last]/wood_total_C
  }
  else {
    params$restart["AbvGrndWood"] <- PEcAn.utils::ud_convert(ens$AbvGrndWood[last], 
                                                             "kg/m^2", "g/m^2")
    wood_total_C <- ens$AbvGrndWood[last] + ens$fine_root_carbon_content[last] + 
      ens$coarse_root_carbon_content[last]
    if (wood_total_C <= 0) 
      wood_total_C <- 1e-04
    params$restart["abvGrndWoodFrac"] <- ens$AbvGrndWood[last]/wood_total_C
    params$restart["coarseRootFrac"] <- ens$coarse_root_carbon_content[last]/wood_total_C
    params$restart["fineRootFrac"] <- ens$fine_root_carbon_content[last]/wood_total_C
  }
  
  if ("GWBI" %in% var.names) {
    forecast[[length(forecast) + 1]] <- PEcAn.utils::ud_convert(mean(ens$GWBI), 
                                                                "kg/m^2/s", "Mg/ha/yr")
    names(forecast[[length(forecast)]]) <- c("GWBI")
  }
  if ("NEE" %in% var.names) {
    forecast[[length(forecast) + 1]] <-
      nee_model_to_obs(mean(ens$NEE))
    names(forecast[[length(forecast)]]) <- "NEE"
  }
  
  if ("Qle" %in% var.names) {
    forecast[[length(forecast) + 1]] <- mean(ens$Qle)
    names(forecast[[length(forecast)]]) <- c("Qle")
  }
  if ("leaf_carbon_content" %in% var.names) {
    forecast[[length(forecast) + 1]] <- ens$leaf_carbon_content[last]
    names(forecast[[length(forecast)]]) <- c("LeafC")
  }
  if ("LAI" %in% var.names) {
    forecast[[length(forecast) + 1]] <- ens$LAI[last]
    names(forecast[[length(forecast)]]) <- c("LAI")
  }
  else {
    params$restart["LAI"] <- ens$LAI[last]
  }
  if ("litter_carbon_content" %in% var.names) {
    forecast[[length(forecast) + 1]] <- ens$litter_carbon_content[last]
    names(forecast[[length(forecast)]]) <- c("litter_carbon_content")
  }
  else {
    params$restart["litter_carbon_content"] <- PEcAn.utils::ud_convert(ens$litter_carbon_content[last], 
                                                                       "kg m-2", "g m-2")
  }
  if ("litter_mass_content_of_water" %in% var.names) {
    forecast[[length(forecast) + 1]] <- ens$litter_mass_content_of_water[last]
    names(forecast[[length(forecast)]]) <- c("litter_mass_content_of_water")
  }
  else {
    params$restart["litter_mass_content_of_water"] <- ens$litter_mass_content_of_water[last]
  }
  if ("SoilMoist" %in% var.names) {
    forecast[[length(forecast) + 1]] <- ens$SoilMoist[last]
    names(forecast[[length(forecast)]]) <- c("SoilMoist")
  }
  else {
    params$restart["SoilMoist"] <- ens$SoilMoist[last]
  }
  if ("SoilMoistFrac" %in% var.names) {
    forecast[[length(forecast) + 1]] <- ens$SoilMoistFrac[last] * 
      100
    names(forecast[[length(forecast)]]) <- c("SoilMoistFrac")
  }
  else {
    params$restart["SoilMoistFrac"] <- ens$SoilMoistFrac[last]
  }
  if ("SWE" %in% var.names) {
    forecast[[length(forecast) + 1]] <- ens$SWE[last]
    names(forecast[[length(forecast)]]) <- c("SWE")
  }
  else {
    params$restart["SWE"] <- ens$SWE[last]/10
  }
  if ("TotLivBiom" %in% var.names) {
    forecast[[length(forecast) + 1]] <- PEcAn.utils::ud_convert(ens$TotLivBiom[last], 
                                                                "kg/m^2", "Mg/ha")
    names(forecast[[length(forecast)]]) <- c("TotLivBiom")
  }
  if ("TotSoilCarb" %in% var.names) {
    forecast[[length(forecast) + 1]] <- ens$TotSoilCarb[last]
    names(forecast[[length(forecast)]]) <- c("TotSoilCarb")
  }
  else {
    params$restart["TotSoilCarb"] <- PEcAn.utils::ud_convert(ens$TotSoilCarb[last], 
                                                             "kg m-2", "g m-2")
  }
  params$restart <- stats::na.omit(params$restart)
  # print(runid)
  X_tmp <- list(X = unlist(forecast), params = params)
  ### Check
  # print(X_tmp)
  return(X_tmp)
  # print("end of write.config.SIPNET")
}

nee_model_to_obs <- function(x) {
  x * 1e8 / 1.157407
}

nee_obs_to_model <- function(x) {
  x * 1.157407 / 1e8
}
