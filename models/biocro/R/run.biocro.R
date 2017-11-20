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
  l2n <- function(x) lapply(x, as.numeric)
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

  hourly.results = list()
  for (i in seq_along(years)) {
    yeari <- years[i]
    metfile <- paste(metpath, yeari, "csv", sep = ".")
    WetDat <- data.table::fread(metfile)
    stopifnot(all(sapply(WetDat, is.numeric)))
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
    if (!is.null(day1) && day1 > lubridate::yday(starti)) {
      lubridate::yday(starti) <- day1
    }
    if (!is.null(dayn) && dayn < lubridate::yday(starti)) {
      lubridate::yday(plant_date) <- dayn
    }
    WetDat <- WetDat[WetDat$doy >= lubridate::yday(starti) & WetDat$doy <= lubridate::yday(endi), ]

    HarvestedYield <- 0

    if (utils::packageVersion('BioCro') >= 1.0) {

      if ("SolarR" %in% names(WetDat)) {
        WetDat <- dplyr::rename(WetDat, solar = "SolarR")
      }
      if ("WS" %in% names(WetDat)) {
        WetDat <- dplyr::rename(WetDat, windspeed = "WS")
      }

      if (i == 1) {
        initial_values <- config$pft$initial_values
      }

      if (i > 1) { # TODO HarvestedYield is never used and coppice not applicable to all crops. Rethink?
        if ((i - 1) %% coppice.interval == 0) {
          # coppice when remainder = 0
          HarvestedYield <- round(data.table::last(tmp.result$Stem) * 0.95, 2)
        } else if ((i - 1) %% coppice.interval == 1) {
          # year after coppice
          initial_values$Stem <- initial_values$Stem * 0.05
        }  # else { # do nothing if neither coppice year nor year following
      }

      tmp.result <- BioCro::Gro(
        initial_values = initial_values,
        parameters = config$pft$parameters,
        varying_parameters = WetDat,
        modules = config$pft$modules)

      # save final state as initial values for next year
      # TODO: Some pools should NOT start at 100% of previous season --
      # need to account for harvest, decomposition, etc
      initial_values <- tmp.result[nrow(tmp.result), colnames(tmp.result) %in% names(config$pft$initial_values)]

      tmp.result <- dplyr::rename(tmp.result,
        ThermalT = "TTc",
        LAI = "lai",
        SoilEvaporation = "soil_evaporation",
        CanopyTrans = "canopy_transpiration")
      tmp.result$AboveLitter = tmp.result$LeafLitter + tmp.result$StemLitter
      tmp.result$BelowLitter = tmp.result$RootLitter + tmp.result$RhizomeLitter

    } else {  # BioCro vesion is less than 1.0.

      # Check that all variables are present in the expected order --
      # BioGro < 1.0 accesses weather vars by position and DOES NOT check headers.
      stopifnot(identical(colnames(WetDat), c("year", "doy", "hour", "SolarR", "Temp", "RH", "WS", "precip")))
      WetDat <- as.matrix(WetDat)

      # BLETCHEROUS HACK: BioCro 0.94 starts the run by subsetting weather data
      # to day1:dayn, but it assumes the data start on DOY 1 and contain
      # (yearlength*(24/timestep)) lines. This means that in practice, day1 and
      # dayn are treated as "day of file" not "day of year".
      # BioCro *does* handle DOY correctly downstream of the subsetting, so here
      # we check if the current BioCro has fixed this assumption.
      # If not, rescale day1 and dayn to be relative to the start of the input.
      #   Scaling is derived by inverting Biocro's day->index equations.
      biocro_checks_doy <- tryCatch(
        {m <- BioCro::BioGro(
          WetDat = matrix(c(0,10,0,0,0,0,0,0), nrow = 1),
          day1 = 10, dayn = 10, timestep = 24);
        class(m) == "BioGro"},
        error = function(e){FALSE})
      if (!biocro_checks_doy && min(WetDat[,"doy"])>1) {
        if (!is.null(day1)){
          # Biocro calculates line number as `indes1 <- (day1 - 1) * 24`
          indes1 <- Position(function(x)x==day1, WetDat[,"doy"])
          day1 <- indes1/24 + 1
        }
        if (!is.null(dayn)){
          # Biocro calculates line number as `indesn <- (dayn) * 24`
          indesn <- Position(function(x)x==dayn, WetDat[,"doy"], right = TRUE)
          dayn <- indesn/24
        }
      }
      
      if (genus == "Saccharum") {
        tmp.result <- BioCro::caneGro(WetDat = WetDat, lat = lat, soilControl = l2n(config$pft$soilControl))
        # Addin Rhizome an Grain to avoid error in subsequent script processing results
        tmp.result$Rhizome <- 0
        tmp.result$Grain <- 0
      } else if (genus == "Salix") {
        if (i == 1) {
          iplant <- config$pft$iPlantControl
        } else {
          iplant$iRhizome <- data.table::last(tmp.result$Rhizome)
          iplant$iRoot <- data.table::last(tmp.result$Root)
          iplant$iStem <- data.table::last(tmp.result$Stem)
          
          if ((i - 1)%%coppice.interval == 0) {
            # coppice when remainder = 0
            HarvestedYield <- round(data.table::last(tmp.result$Stem) * 0.95, 2)
          } else if ((i - 1)%%coppice.interval == 1) {
            # year after coppice
            iplant$iStem <- iplant$iStem * 0.05
          }  # else { # do nothing if neither coppice year nor year following
        }
        ## run willowGro
        
        tmp.result <- BioCro::willowGro(
          WetDat = WetDat,
          iRhizome = as.numeric(iplant$iRhizome),
          iRoot = as.numeric(iplant$iRoot),
          iStem = as.numeric(iplant$iStem),
          day1 = day1, dayn = dayn,
          soilControl = l2n(config$pft$soilControl),
          canopyControl = l2n(config$pft$canopyControl),
          willowphenoControl = l2n(config$pft$phenoParms),
          seneControl = l2n(config$pft$seneControl),
          photoControl = l2n(config$pft$photoParms))

      } else if (genus == "Miscanthus") {
        if (yeari == years[1]) {
          iRhizome <- config$pft$iPlantControl$iRhizome
        } else {
          iRhizome <- data.table::last(tmp.result$Rhizome)
          HarvestedYield <- round(data.table::last(tmp.result$Stem) * 0.95, 2)
        }
        ## run BioGro
        tmp.result <- BioCro::BioGro(
          WetDat = WetDat,
          day1 = day1,
          dayn = dayn, soilControl = l2n(config$pft$soilControl),
          canopyControl = l2n(config$pft$canopyControl),
          phenoControl = l2n(config$pft$phenoParms),
          seneControl = l2n(config$pft$seneControl),
          iRhizome = as.numeric(iRhizome),
          photoControl = config$pft$photoParms)

      } else if (genus == "Sorghum") {
        ## run BioGro
        tmp.result <- BioCro::BioGro(
          WetDat = WetDat,
          day1 = day1,
          dayn = dayn,
          soilControl = l2n(config$pft$soilControl),
          canopyControl = l2n(config$pft$canopyControl),
          phenoControl = l2n(config$pft$phenoParms),
          seneControl = l2n(config$pft$seneControl),
          photoControl = l2n(config$pft$photoParms))

      }
      names(tmp.result) <- sub("DayofYear", "doy", names(tmp.result))
      names(tmp.result) <- sub("Hour", "hour", names(tmp.result))

    } # end BioCro version < 1.0

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
