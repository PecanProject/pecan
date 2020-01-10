
l2n <- function(x) lapply(x, as.numeric)

# wrapper to encapsulate version-specific logic for BioCro 0.9x
# not exported
call_biocro_0.9 <- function(WetDat, genus, year_in_run,
                            config, lat, lon,
                            tmp.result, HarvestedYield) {

  # Check that all variables are present in the expected order --
  # BioGro < 1.0 accesses weather vars by position and DOES NOT check headers.
  expected_cols <- c("year", "doy", "hour", "[Ss]olar", "Temp", "RH", "WS|windspeed", "precip")
  if(!all(mapply(grepl, expected_cols, colnames(WetDat)))){
    PEcAn.logger::logger.severe("Format error in weather file: Columns must be (", expected_cols, "), in that order.")
  }
  day1 <- min(WetDat$doy) # data already subset upstream, but BioCro 0.9 assumes a full year if day1/dayn are unset
  dayn <- max(WetDat$doy)
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
    inherits(m, "BioGro") },
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

  coppice.interval = config$pft$coppice.interval
  if(is.null(coppice.interval)) {
     coppice.interval = 1 # i.e. harvest every year
  }

  if (genus == "Saccharum") {
    tmp.result <- BioCro::caneGro(
      WetDat = WetDat,
      lat = lat,
      soilControl = l2n(config$pft$soilControl))
    # Addin Rhizome an Grain to avoid error in subsequent script processing results
    tmp.result$Rhizome <- 0
    tmp.result$Grain <- 0
  } else if (genus %in% c("Salix", "Populus")) {
    if (year_in_run == 1) {
      iplant <- config$pft$iPlantControl
    } else {
      iplant$iRhizome <- data.table::last(tmp.result$Rhizome)
      iplant$iRoot <- data.table::last(tmp.result$Root)
      iplant$iStem <- data.table::last(tmp.result$Stem)

      if ((year_in_run - 1)%%coppice.interval == 0) {
        # coppice when remainder = 0
        HarvestedYield <- round(data.table::last(tmp.result$Stem) * 0.95, 2)
      } else if ((year_in_run - 1)%%coppice.interval == 1) {
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
      day1 = day1,
      dayn = dayn,
      soilControl = l2n(config$pft$soilControl),
      canopyControl = l2n(config$pft$canopyControl),
      willowphenoControl = l2n(config$pft$phenoParms),
      seneControl = l2n(config$pft$seneControl),
      photoControl = l2n(config$pft$photoParms))

  } else if (genus %in% c("Miscanthus", "Panicum")) {
    if (year_in_run == 1) {
      iRhizome <- config$pft$iPlantControl$iRhizome
    } else {
      iRhizome <- data.table::last(tmp.result$Rhizome)
      HarvestedYield <- round(data.table::last(tmp.result$Stem) * 0.95, 2)
    }
    ## run BioGro
    tmp.result <- BioCro::BioGro(
      WetDat = WetDat,
      day1 = day1,
      dayn = dayn,
      soilControl = l2n(config$pft$soilControl),
      canopyControl = l2n(config$pft$canopyControl),
      phenoControl = l2n(config$pft$phenoParms),
      seneControl = l2n(config$pft$seneControl),
      iRhizome = as.numeric(iRhizome),
      photoControl = config$pft$photoParms)

  } else if (genus %in% c("Sorghum", "Setaria")) {
    if (year_in_run == 1) {
      iplant <- config$pft$iPlantControl
    } else {
      iplant$iRhizome <- data.table::last(tmp.result$Rhizome)
      iplant$iRoot <- data.table::last(tmp.result$Root)
      iplant$iStem <- data.table::last(tmp.result$Stem)
    }
    ## run BioGro
    tmp.result <- BioCro::BioGro(
      WetDat = WetDat,
      iRhizome = as.numeric(iplant$iRhizome),
      iRoot = as.numeric(iplant$iRoot),
      iStem = as.numeric(iplant$iStem),
      iLeaf = as.numeric(iplant$iLeaf), 
      day1 = day1,
      dayn = dayn,
      soilControl = l2n(config$pft$soilControl),
      canopyControl = l2n(config$pft$canopyControl),
      phenoControl = l2n(config$pft$phenoParms),
      seneControl = l2n(config$pft$seneControl),
      photoControl = l2n(config$pft$photoParms))

  } else {
    PEcAn.logger::logger.severe(
      "Genus '", genus, "' is not supported by PEcAn.BIOCRO when using BioCro 0.9x.",
      "Supported genera: Saccharum, Salix, Populus, Sorghum, Miscanthus, Panicum, Setaria")
  }
  names(tmp.result) <- sub("DayofYear", "doy", names(tmp.result))
  names(tmp.result) <- sub("Hour", "hour", names(tmp.result))

  list(tmp.result = tmp.result, HarvestedYield = HarvestedYield)
} # call_biocro_0.9




# wrapper to encapsulate version-specific logic for BioCro 1.x
# not exported
call_biocro_1 <- function(WetDat, genus, year_in_run,
                          config, lat, lon,
                          tmp.result, HarvestedYield) {

  if (year_in_run == 1) {
    initial_values <- config$pft$initial_values
  } else {
    # Use final state from last year as initial values
    # TODO: Some pools should NOT start at 100% of previous season --
    # need to account for harvest, decomposition, etc
    initial_values <- tmp.result[nrow(tmp.result), colnames(tmp.result) %in% names(config$pft$initial_values)]
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

  list(tmp.result = tmp.result, HarvestedYield = round(data.table::last(tmp.result$Stem) * 0.95, 2))
} # call_biocro_1
