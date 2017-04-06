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
#' @importFrom PEcAn.data.land get.soil
#' @import data.table
#' @author David LeBauer
run.biocro <- function(lat, lon, metpath, soil.nc = NULL, config = config, coppice.interval = 1) {
  l2n <- function(x) lapply(x, as.numeric)
  start.date <- lubridate::ymd(config$run$start.date)
  end.date   <- lubridate::ymd(config$run$end.date)
  genus <- config$pft$type$genus
  years <- lubridate::year(start.date):lubridate::year(end.date)

  if (!is.null(soil.nc)) {
    soil <- get.soil(lat = lat, lon = lon, soil.nc = soil.nc)
    config$pft$soilControl$soilType <- ifelse(soil$usda_class %in% 1:10, 
                                              soil$usda_class, 
                                              10)
    config$pft$soilControl$soilDepth <- soil$ref_depth
  }
  
  hourly.results = list()
  for (i in seq_along(years)) {
    yeari <- years[i]
    starti <- max(start.date, lubridate::ymd(paste0(yeari, "-01-01")))
    endi <- min(end.date, lubridate::ymd(paste0(yeari, "-12-31")))
    metfile <- paste(metpath, starti, endi, "csv", sep=".")
    WetDat <- fread(metfile)

    # Check that all variables are present in the expected order --
    # BioGro accesses weather vars by position and DOES NOT check headers.
    stopifnot(identical(colnames(WetDat), c("year", "doy", "hour", "SolarR", "Temp", "RH", "WS", "precip")))
    stopifnot(all(sapply(WetDat, is.numeric)))
    WetDat <- as.matrix(WetDat)
    
    if (!is.null(config$simulationPeriod)) {
      day1 <- lubridate::yday(config$simulationPeriod$dateofplanting)
      dayn <- lubridate::yday(config$simulationPeriod$dateofharvest)
    } else if (lat > 0) {
      day1 <- as.numeric(as.data.table(WetDat)[doy < 180 & Temp < -2, list(day1 = max(doy))])
      dayn <- as.numeric(as.data.table(WetDat)[doy > 180 & Temp < -2, list(day1 = min(doy))])
      ## day1 = last spring frost dayn = first fall frost from Miguez et al 2009
    } else {
      day1 <- NULL
      dayn <- NULL
    }
    
    HarvestedYield <- 0
    if (genus == "Saccharum") {
      tmp.result <- BioCro::caneGro(WetDat = WetDat, lat = lat, soilControl = l2n(config$pft$soilControl))
      # Addin Rhizome an Grain to avoid error in subsequent script processing results
      tmp.result$Rhizome <- 0
      tmp.result$Grain <- 0
    } else if (genus == "Salix") {
      if (i == 1) {
        iplant <- config$pft$iPlantControl
      } else {
        iplant$iRhizome <- last(tmp.result$Rhizome)
        iplant$iRoot <- last(tmp.result$Root)
        iplant$iStem <- last(tmp.result$Stem)
        
        if ((i - 1)%%coppice.interval == 0) {
          # coppice when remainder = 0
          HarvestedYield <- round(last(tmp.result$Stem) * 0.95, 2)
        } else if ((i - 1)%%coppice.interval == 1) 
        {
          # year after coppice
          iplant$iStem <- iplant$iStem * 0.05
        }  # else { # do nothing if neither coppice year nor year following
      }
      ## run willowGro
      
      tmp.result <- BioCro::willowGro(WetDat = WetDat,
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
        iRhizome <- last(tmp.result$Rhizome)
        HarvestedYield <- round(last(tmp.result$Stem) * 0.95, 2)
      }
      ## run BioGro
      tmp.result <- BioCro::BioGro(WetDat = WetDat,
                           day1 = day1, 
                           dayn = dayn, soilControl = l2n(config$pft$soilControl), 
                           canopyControl = l2n(config$pft$canopyControl), 
                           phenoControl = l2n(config$pft$phenoParms), 
                           seneControl = l2n(config$pft$seneControl),
                           iRhizome = as.numeric(iRhizome), 
                           photoControl = config$pft$photoParms)
      
    } else if (genus == "Sorghum") {
      ## run BioGro
      tmp.result <- BioCro::BioGro(WetDat = WetDat,
                           day1 = day1, 
                           dayn = dayn, 
                           soilControl = l2n(config$pft$soilControl), 
                           canopyControl = l2n(config$pft$canopyControl),
                           phenoControl = l2n(config$pft$phenoParms), 
                           seneControl = l2n(config$pft$seneControl),
                           photoControl = l2n(config$pft$photoParms))
      
    }
    # TODO return the whole BioGro result instead of selected columns?
    result.yeari.hourly <- with(tmp.result, data.table(year = yeari,
                                                       doy = DayofYear,
                                                       hour = Hour, ThermalT,
                                                       Stem, Leaf, Root, 
                                                       AboveLitter, BelowLitter,
                                                       Rhizome, Grain, 
                                                       LAI, SoilEvaporation, 
                                                       CanopyTrans,
                                                       key = c("year", "doy", "hour")))
    result.yeari.withmet <- merge(x = result.yeari.hourly,
                                  y = WetDat, by = c("year", "doy", "hour"))
    hourly.results[[i]] <- result.yeari.withmet
  }
  
  hourly.results <- do.call("rbind", hourly.results)
  hourly.results <- hourly.results[order(year, doy, hour)]
  
  daily.results <- hourly.results[, list(Stem = max(Stem), 
                                         Leaf = max(Leaf),
                                         Root = max(Root), 
                                         AboveLitter = max(AboveLitter),
                                         BelowLitter = max(BelowLitter),
                                         Rhizome = max(Rhizome),
                                         SoilEvaporation = sum(SoilEvaporation), 
                                         CanopyTrans = sum(CanopyTrans), 
                                         Grain = max(Grain), 
                                         LAI = max(LAI), 
                                         tmax = max(Temp), tmin = min(Temp), tavg = mean(Temp), 
                                         precip = sum(precip)), by = "year,doy"]
  
  annual.results <- hourly.results[, list(Stem = max(Stem),
                                          Leaf = max(Leaf), 
                                          Root = max(Root), 
                                          AboveLitter = max(AboveLitter),
                                          BelowLitter = max(BelowLitter),
                                          Rhizome = max(Rhizome), 
                                          Grain = max(Grain), 
                                          SoilEvaporation = sum(SoilEvaporation), 
                                          CanopyTrans = sum(CanopyTrans), 
                                          map = sum(precip), mat = mean(Temp)),
                                   by = "year"]
  return(list(hourly = hourly.results, 
              daily = daily.results, 
              annually = data.table(lat = lat, lon = lon, annual.results)))
} # run.biocro
