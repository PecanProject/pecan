#' Run BioCro at a point
#'
#' @param lat latitude in decimal degrees
#' @param lon longitude in decimal degrees
#' @param metpath full path and name prefix of a csv file with hourly data in BioCro format,
#' e.g. `/dir/met` if the files to be used are `/dir/met.2004.csv` and `dir/met.2005.csv'
#' @param soil.nc full path and name of a netCDF file with soil data
#' @param config full path and name of a config.xml file containing parameter values and configuration information for BioCro
#' @param coppice.interval numeric, number of years between cuttings for coppice plant or perinneal grass (default 1)
#' @return output from one of the \code{BioCro::*.Gro} functions (determined by \code{config$genus}), as data.table object
#' @export
#' @author David LeBauer
run.biocro <- function(lat, lon, metpath, soil.nc = NULL, config = config, coppice.interval = 1) {

  start.date <- lubridate::date(config$run$start.date)
  end.date   <- lubridate::date(config$run$end.date)
  genus <- config$pft$type$genus
  years <- lubridate::year(start.date):lubridate::year(end.date)

  if (!is.null(soil.nc)) {
    soil <- PEcAn.data.land::get.soil(lat = lat, lon = lon, soil.nc = soil.nc)
    config$pft$soilControl$soilType <- ifelse(soil$usda_class %in% 1:10, 
                                              soil$usda_class, 
                                              10)
    config$pft$soilControl$soilDepth <- soil$ref_depth
  }

  if (utils::packageVersion('BioCro') >= 1.0) {
    caller_fn <- call_biocro_1
  } else {
    caller_fn <- call_biocro_0.9
  }

  hourly.results <- list()
  for (i in seq_along(years)) {
    yeari <- years[i]
    metfile <- paste(metpath, yeari, "csv", sep = ".")
    WetDat <- data.table::fread(metfile)
    if(!all(sapply(WetDat, is.numeric))){
      PEcAn.logger::logger.severe("Format error in weather file: All columns must be numeric, but got (", sapply(WetDat, class), ")")
    }

    # Simulation for current year starts on the latest of:
    # First day of whole model run, Jan 1 of current year, planting date, (last frost if planting date unset)
    starti <- max(start.date, lubridate::ymd(paste0(yeari, "-01-01")))
    endi <- min(end.date, lubridate::ymd(paste0(yeari, "-12-31")))
    if (!is.null(config$simulationPeriod)) {
      day1 <- lubridate::yday(config$simulationPeriod$dateofplanting)
      dayn <- lubridate::yday(config$simulationPeriod$dateofharvest)
    } else if (lat > 0) {
      day1 <- max(WetDat[ (WetDat[,"doy"] < 180 & WetDat[,"Temp"] < -2), "doy"])
      dayn <- min(WetDat[ (WetDat[,"doy"] > 180 & WetDat[,"Temp"] < -2), "doy"])
      ## day1 = last spring frost dayn = first fall frost from Miguez et al 2009
    } else {
      day1 <- NULL
      dayn <- NULL
    }
    WetDat <- WetDat[
      WetDat$doy >= max(day1, lubridate::yday(starti))
      & WetDat$doy <= min(dayn, lubridate::yday(endi)), ]

    HarvestedYield <- 0

    call_result <- caller_fn(
      WetDat = WetDat,
      years = years, yeari = yeari, i = i,
      config = config, genus = genus,
      lat = lat, lon = lon,
      coppice.interval = coppice.interval,
      tmp.result = tmp.result,
      HarvestedYield = HarvestedYield)

    tmp.result <- call_result$tmp.result
    HarvestedYield <- call_result$HarvestedYield

    result.yeari.hourly <- with(tmp.result,
      data.table::data.table(
        year = yeari,
        doy, hour, ThermalT,
        Stem, Leaf, Root,
        AboveLitter, BelowLitter,
        Rhizome, Grain, LAI,
        SoilEvaporation, CanopyTrans,
        key = c("year", "doy", "hour")))
    result.yeari.withmet <- merge(x = result.yeari.hourly,
                                  y = WetDat, by = c("year", "doy", "hour"))
    hourly.results[[i]] <- result.yeari.withmet
  }

  
  hourly.results <- do.call("rbind", hourly.results)
  hourly.results <- hourly.results[order(hourly.results$year, hourly.results$doy, hourly.results$hour),]

  # Compute daily and yearly results by taking max or sum as appropriate.
  # This notation could be more compact if we used nonstandard evaluation
  # with bare variable names, but this way works and ensures that
  # `R CMD check` doesn't complain about undefined variables.
  hourly_grp <- dplyr::group_by_at(.tbl = hourly.results, .vars = c("year", "doy"))
  daily.results <- dplyr::bind_cols(
    dplyr::summarize_at(
      .tbl = hourly_grp,
      .vars = c("Stem", "Leaf", "Root", "AboveLitter", "BelowLitter",
                "Rhizome", "Grain", "LAI", tmax = "Temp"),
      .fun = max),
    dplyr::summarize_at(
      .tbl = hourly_grp,
      .vars = c("SoilEvaporation", "CanopyTrans", "precip"),
      .fun = sum),
    dplyr::summarize_at(
      .tbl = hourly_grp,
      .vars = c(tmin = "Temp"),
      .fun = min),
    dplyr::summarize_at(
      .tbl = hourly_grp,
      .vars = c(tavg = "Temp"),
      .fun = mean))
  # bind_cols on 4 tables leaves 3 sets of duplicate year and day columns.
  # Let's drop these.
  col_order <- c("year", "doy", "Stem", "Leaf", "Root",
                 "AboveLitter", "BelowLitter", "Rhizome",
                 "SoilEvaporation", "CanopyTrans", "Grain", "LAI",
                 "tmax", "tmin", "tavg", "precip")
  daily.results <- daily.results[, col_order]
  
  daily_grp <- dplyr::group_by_at(.tbl = hourly.results, .vars = "year")
  annual.results <- dplyr::bind_cols(
    dplyr::summarize_at(
      .tbl = daily_grp,
      .vars = c("Stem", "Leaf", "Root", "AboveLitter", "BelowLitter",
                "Rhizome", "Grain"),
      .fun = max),
    dplyr::summarize_at(
      .tbl = daily_grp,
      .vars = c("SoilEvaporation", "CanopyTrans", map = "precip"),
      .fun = sum),
    dplyr::summarize_at(
      .tbl = daily_grp,
      .vars = c(mat = "Temp"),
      .fun = mean))
  col_order <- c("year", "Stem", "Leaf", "Root", "AboveLitter", "BelowLitter",
                 "Rhizome", "Grain", "SoilEvaporation", "CanopyTrans",
                 "map", "mat")
  annual.results <- annual.results[, col_order]

  return(list(hourly = hourly.results, 
              daily = daily.results, 
              annually = data.table::data.table(lat = lat, lon = lon, annual.results)))
} # run.biocro
