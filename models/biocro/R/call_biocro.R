
l2n <- function(x) lapply(x, as.numeric)

# wrapper to encapsulate version-specific logic for BioCro 0.9x
# not exported
call_biocro_0.9 <- function(WetDat, day1, dayn, years, yeari, i,
                            config, genus, lat, lon, coppice.interval,
                            tmp.result, HarvestedYield) {

  # Check that all variables are present in the expected order --
  # BioGro < 1.0 accesses weather vars by position and DOES NOT check headers.
  expected_cols <- c("year", "doy", "hour", "SolarR", "Temp", "RH", "WS", "precip")
  if(!identical(colnames(WetDat), expected_cols)){
    PEcAn.logger::logger.severe("Format error in weather file: Columns must be (", expected_cols, "), in that order.")
  }
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

  list(tmp.result = tmp.result, HarvestedYield = HarvestedYield)
} # call_biocro_0.9




# wrapper to encapsulate version-specific logic for BioCro 1.x
# not exported
call_biocro_1 <- function(WetDat, day1, dayn, years, yeari, i,
                          config, genus, lat, lon, coppice.interval,
                          tmp.result, HarvestedYield) {

  if ("SolarR" %in% names(WetDat)) {
    WetDat <- dplyr::rename(WetDat, solar = "SolarR")
  }
  if ("WS" %in% names(WetDat)) {
    WetDat <- dplyr::rename(WetDat, windspeed = "WS")
  }

  if (i == 1) {
    initial_values <- config$pft$initial_values
  } else {
    # Use final state from last year as initial values
    # TODO: Some pools should NOT start at 100% of previous season --
    # need to account for harvest, decomposition, etc
    initial_values <- tmp.result[nrow(tmp.result), colnames(tmp.result) %in% names(config$pft$initial_values)]
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

  tmp.result <- dplyr::rename(tmp.result,
    ThermalT = "TTc",
    LAI = "lai",
    SoilEvaporation = "soil_evaporation",
    CanopyTrans = "canopy_transpiration")
  tmp.result$AboveLitter <- tmp.result$LeafLitter + tmp.result$StemLitter
  tmp.result$BelowLitter <- tmp.result$RootLitter + tmp.result$RhizomeLitter

  list(tmp.result = tmp.result, HarvestedYield = HarvestedYield)
} # call_biocro_1
