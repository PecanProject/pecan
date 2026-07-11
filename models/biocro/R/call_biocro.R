
l2n <- function(x) lapply(x, as.numeric)

# wrapper to encapsulate version-specific logic for BioCro 0.9x
# not exported
call_biocro_0.9 <- function(WetDat, genus, year_in_run,
                            config, lat, lon,
                            tmp.result, HarvestedYield) {

  # Check that all variables are present in the expected order --
  # BioGro < 1.0 accesses weather vars by position and DOES NOT check headers.
  expected_cols <- c("year", "doy", "hour", "[Ss]olar", "Temp", "RH", "WS|windspeed", "precip")
  if (!all(mapply(grepl, expected_cols, colnames(WetDat)))) {
    PEcAn.logger::logger.severe("Format error in weather file: Columns must be (", expected_cols, "), in that order.")
  }
  
  if (length(unique(WetDat[, "year"])) > 1) {
    PEcAn.logger::logger.severe("WetDat must contain only one year of data when using BioCro 0.9")
  }
  
  n_days <- length(unique(WetDat[, "doy"]))
  if (nrow(WetDat) != 24 * n_days) {
    PEcAn.logger::logger.severe("WetDat must have exactly 24 rows per day (hourly timestep) when using BioCro 0.9")
  }
  
  day1 <- min(WetDat$doy)
  dayn <- max(WetDat$doy)
  WetDat <- as.matrix(WetDat)

  # BioCro 0.9x treats day1/dayn as "day of file" not "day of year",
  # so rescale to be relative to the start of the input when data doesn't start on DOY 1.
  if (min(WetDat[, "doy"]) > 1) {
    day1 <- 1
    dayn <- n_days
  }

  coppice.interval <- config$pft$coppice.interval
  if (is.null(coppice.interval)) {
    coppice.interval <- 1 # i.e. harvest every year
  }

  if (genus == "Saccharum") {
    # probably should be handled like coppice shrubs or perennial grasses
    tmp.result <- BioCro::caneGro(
      WetDat = WetDat,
      lat = lat,
      soilControl = l2n(config$pft$soilControl)
    )
    # Addin Rhizome and Grain to avoid error in subsequent script processing results
    tmp.result$Rhizome <- 0
    tmp.result$Grain <- 0
  } else if (genus %in% c("Salix", "Populus")) { # coppice trees / shrubs
    if (year_in_run == 1) {
      iplant <- config$pft$iPlantControl
    } else {
      iplant$iRhizome <- data.table::last(tmp.result$Rhizome)
      iplant$iRoot <- data.table::last(tmp.result$Root)
      iplant$iStem <- data.table::last(tmp.result$Stem)

      if ((year_in_run - 1) %% coppice.interval == 0) {
        # coppice when remainder = 0
        HarvestedYield <- round(data.table::last(tmp.result$Stem) * 0.95, 2)
      } else if ((year_in_run - 1) %% coppice.interval == 1) {
        # year after coppice
        iplant$iStem <- iplant$iStem * 0.05
      } # else { # do nothing if neither coppice year nor year following
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
      photoControl = l2n(config$pft$photoParms)
    )
  } else if (genus %in% c("Miscanthus", "Panicum")) { # perennial grasses
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
      photoControl = config$pft$photoParms
    )
  } else if (genus %in% c("Sorghum", "Setaria")) { # annual grasses
    # Perennial Sorghum exists but is not a major crop
    # assume these are replanted from seed each year
    # https://landinstitute.org/our-work/perennial-crops/perennial-sorghum/
    iplant <- config$pft$iPlantControl
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
      photoControl = l2n(config$pft$photoParms)
    )
  } else {
    PEcAn.logger::logger.severe(
      "Genus '", genus, "' is not supported by PEcAn.BIOCRO when using BioCro 0.9x.",
      "Supported genera: Saccharum, Salix, Populus, Sorghum, Miscanthus, Panicum, Setaria"
    )
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
    modules = config$pft$modules
  )

  tmp.result <- dplyr::rename(tmp.result,
    ThermalT = "TTc",
    LAI = "lai",
    SoilEvaporation = "soil_evaporation",
    CanopyTrans = "canopy_transpiration"
  )
  tmp.result$AboveLitter <- tmp.result$LeafLitter + tmp.result$StemLitter
  tmp.result$BelowLitter <- tmp.result$RootLitter + tmp.result$RhizomeLitter

  list(tmp.result = tmp.result, HarvestedYield = round(data.table::last(tmp.result$Stem) * 0.95, 2))
} # call_biocro_1
