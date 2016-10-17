#' Run BioCro at a point
#'
#' @param lat latitude in decimal degrees
#' @param lon longitude in decimal degrees
#' @param metfile full path and name of a netCDF file in either a PEcAn-CF format with meteorological driver data
#' or a csv file with hourly data in BioCro format
#' @param soil.nc full path and name of a netCDF file with soil data
#' @param config full path and name of a config.xml file containing parameter values and configuration information for BioCro
#' @param coppice.interval numeric, number of years between cuttings for coppice plant or perinneal grass (default 1)
#' @return output from one of the \code{BioCro::*.Gro} functions (determined by \code{config$genus}), as data.table object
#' @export
#' @author David LeBauer
run.biocro <- function(lat, lon, metfile, soil.nc = NULL, config = config, coppice.interval = 1, 
                       met.uncertainty = FALSE, irrigation = FALSE) {
  library(data.table)
  library(lubridate)
  l2n <- function(x) lapply(x, as.numeric)
  start.date <- ceiling_date(as.POSIXct(config$run$start.date), "day")
  end.date   <- floor_date(as.POSIXct(config$run$end.date), "day")
  genus <- config$pft$type$genus
  years <- year(start.date):year(end.date)
  ## Meteorology
  if (grepl(".nc$", basename(metfile))) {
    if (met.uncertainty == TRUE) {
      start.date <- "1979-01-01"
      end.date <- "2010-12-31"
      years <- sample(year(start.date):year(end.date), size = 15, replace = TRUE)
    }
    
    met.nc <- nc_open(metfile)
    met <- load.cfmet(met.nc, lat = lat, lon = lon, 
                      start.date = start.date, end.date = end.date)
    if (met.uncertainty == TRUE) {
      met <- met[year %in% years]
    }
    
    dt <- as.numeric(mean(diff(met$date)))
    
    if (dt > 1) {
      met <- cfmet.downscale.time(cfmet = met, output.dt = 1)
    }
    
    ## add irrigation
    if (irrigation) {
      # 1 mm / hr = 24 mm / d every seven days
      met[, `:=`(precipitation_flux = ifelse(doy %in% seq(7, 364, by = 7), 
                                             precipitation_flux + 1/3600, precipitation_flux))]
    }
    
    biocro.met <- cf2biocro(met)
  } else if (grepl(".csv$", metfile)) {
    biocro.met <- fread(metfile)
  }
  
  if (!is.null(soil.nc)) {
    soil <- get.soil(lat = lat, lon = lon, soil.nc = soil.nc)
    config$pft$soilControl$soilType <- ifelse(soil$usda_class %in% 1:10, 
                                              soil$usda_class, 
                                              10)
    config$pft$soilControl$soilDepth <- soil$ref_depth
  }
  
  for (i in seq_along(years)) {
    yeari <- years[i]
    yearindex <- i * 10000 + yeari  ## for use with met uncertainty
    WetDat <- biocro.met[biocro.met$year == yeari, ]
    
    if (!is.null(config$simulationPeriod)) {
      day1 <- yday(config$simulationPeriod$dateofplanting)
      dayn <- yday(config$simulationPeriod$dateofharvest)
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
      tmp.result <- caneGro(WetDat = WetDat, lat = lat, soilControl = soilP)
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
      
      tmp.result <- willowGro(WetDat = WetDat, 
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
      tmp.result <- BioGro(WetDat = WetDat,
                           day1 = day1, 
                           dayn = dayn, soilControl = l2n(config$pft$soilControl), 
                           canopyControl = l2n(config$pft$canopyControl), 
                           phenoControl = l2n(config$pft$phenoParms), 
                           seneControl = l2n(config$pft$seneControl),
                           iRhizome = as.numeric(iRhizome), 
                           photoControl = config$pft$photoParms)
      
    } else if (genus == "Sorghum") {
      ## run BioGro
      tmp.result <- BioGro(WetDat = WetDat, 
                           day1 = day1, 
                           dayn = dayn, 
                           soilControl = l2n(config$pft$soilControl), 
                           canopyControl = l2n(config$pft$canopyControl),
                           phenoControl = l2n(config$pft$phenoParms), 
                           seneControl = l2n(config$pft$seneControl),
                           photoControl = l2n(config$pft$photoParms))
      
    }
    result.yeari.hourly <- with(tmp.result, data.table(yearindex = yearindex, 
                                                       year = yeari,
                                                       doy = DayofYear,
                                                       hour = Hour, ThermalT,
                                                       Stem, Leaf, Root, 
                                                       Rhizome, Grain, 
                                                       LAI, SoilEvaporation, 
                                                       CanopyTrans,
                                                       key = c("year", "doy", "hour")))
    if (i == 1) {
      hourly.results <- result.yeari.hourly
    } else if (i > 1) {
      hourly.results <- rbind(hourly.results, result.yeari.hourly)
    }
  }
  biocro.met.dt <- as.data.table(biocro.met)
  setkeyv(biocro.met.dt, c("year", "doy", "hour"))
  setkeyv(hourly.results, c("year", "doy", "hour"))
  
  hourly.results <- merge(biocro.met.dt, hourly.results)  ## right join
  hourly.results <- hourly.results[order(yearindex, doy, hour)]
  
  daily.results <- hourly.results[, list(Stem = max(Stem), 
                                         Leaf = max(Leaf),
                                         Root = max(Root), 
                                         Rhizome = max(Rhizome),
                                         SoilEvaporation = sum(SoilEvaporation), 
                                         CanopyTrans = sum(CanopyTrans), 
                                         Grain = max(Grain), 
                                         LAI = max(LAI), 
                                         tmax = max(Temp), tmin = min(Temp), tavg = mean(Temp), 
                                         precip = sum(precip)), by = "yearindex,doy"]
  
  annual.results <- hourly.results[, list(Stem = max(Stem),
                                          Leaf = max(Leaf), 
                                          Root = max(Root), 
                                          Rhizome = max(Rhizome), 
                                          Grain = max(Grain), 
                                          SoilEvaporation = sum(SoilEvaporation), 
                                          CanopyTrans = sum(CanopyTrans), 
                                          map = sum(precip), mat = mean(Temp)),
                                   by = "yearindex"]
  return(list(hourly = hourly.results, 
              daily = daily.results, 
              annually = data.table(lat = lat, lon = lon, annual.results)))
} # run.biocro
