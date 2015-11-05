#' Run BioCro at a point
#'
#' @param lat latitude in decimal degrees
#' @param lon longitude in decimal degrees
#' @param met.nc full path and name of a netCDF file in PEcAn-CF format with meteorological driver data 
#' @param soil.nc full path and name of a netCDF file with soil data
#' @param config full path and name of a config.xml file containing parameter values and configuration information for BioCro 
#' @param coppice.interval numeric, number of years between cuttings for coppice plant or perinneal grass (default 1) 
#' @return output from one of the \code{BioCro::*.Gro} functions (determined by \code{config$genus}), as data.table object
#' @export
#' @author David LeBauer
run.biocro <- function(lat, lon, met.nc = met.nc, 
                       soil.nc = NULL, 
                       config = config,
                       coppice.interval = 1){
  require(data.table)
  require(lubridate)
  start.date <- ceiling_date(as.POSIXct(config$simulationPeriod$dateofplanting), "day")
  end.date <- floor_date(as.POSIXct(config$simulationPeriod$dateofharvest), "day")
  genus <- config$pft$type$genus

  ## Meteorology
  met <- load.cfmet(met.nc, lat = lat, lon = lon, start.date = start.date, end.date = end.date)
  met.hr <- cfmet.downscale.time(cfmet = met, output.dt = 1)
  biocro.met <- cf2biocro(met.hr)
  

  if(!is.null(soil.nc)){
    soil <- get.soil(lat = lat, lon = lon, soil.nc = soil.nc)
    soil.type <- ifelse(soil$usda_class %in% 1:10, soil$usda_class, 10)  
  } else {
    soil.type <- NA
  }
  if(!is.na(soil.type)) {
      config$pft$soilControl$soilType <- soil.type
  }
  soil.parms <- lapply(config$pft$soilControl, as.numeric)
  
  years <- year(start.date):year(end.date)
  for(yeari in years){
    yearchar <- as.character(yeari)
    WetDat <- biocro.met[biocro.met$year == yeari, ]

    ## day1 = last spring frost
    ## dayn = first fall frost from Miguez et al 2009
    if(as.numeric(config$location$latitude) > 0) {
      day1 <-  as.numeric(as.data.table(WetDat)[doy < 180 & Temp < 0, list(day1 = max(doy))])
      dayn <-  as.numeric(as.data.table(WetDat)[doy > 180 & Temp < 0, list(day1 = min(doy))])
    } else if (as.numeric(config$location$latitude) < 0){
      day1 <- NULL
      dayn <- NULL
    }

    HarvestedYield <- 0
    if(genus == "Saccharum") {
      tmp.result<-caneGro(WetDat=WetDat, lat=lat, soilControl=soilP)
      # Addin Rhizome an Grain to avoid error in subsequent script processing results
      tmp.result$Rhizome <- 0
      tmp.result$Grain <- 0
    } else if (genus == "Salix") {
      if(yeari == min(years)){
        iplant <- iwillowParms(iRhizome=1.0, iStem=1.0, iLeaf=0.0,
                               iRoot=1.0, ifrRhizome=0.01, ifrStem=0.01,
                               ifrLeaf = 0.0, ifrRoot = 0.0)
      } else {
      	iplant$iRhizome <- last(tmp.result$Rhizome)
        iplant$iRoot <- last(tmp.result$Root)
        iplant$iStem <- last(tmp.result$Stem)

        if ((yeari - min(years))  %% coppice.interval == 0) { # coppice when remainder = 0
          HarvestedYield  <- round(last(tmp.result$Stem) * 0.95, 2)                
        } else if ((yeari - min(years))  %% coppice.interval == 1) { # year after coppice
          iplant$iStem <- iplant$iStem * 0.05
        } # else { # do nothing if neither coppice year nor year following
      }
      ## run willowGro
      tmp.result <- willowGro(WetDat = WetDat,
                              day1 = day1, dayn = dayn,
                              soilControl = soilParms(soilType = soil.type),
                              canopyControl = config$pft$canopyControl,
                              willowphenoControl = config$pft$phenoParms,
                              seneControl = config$pft$seneControl,
                              iPlantControl = iplant,
                              photoControl=config$pft$photoParms)
      
    } else if (genus == "Miscanthus"){
      if(yeari == min(years)){
        iRhizome <- config$pft$iPlantControl$iRhizome
      } else {
        iRhizome <- last(tmp.result$Rhizome)
        HarvestedYield  <- round(last(tmp.result$Stem) * 0.95, 2)                
      }
      ## run BioGro
      tmp.result <- BioGro(WetDat = WetDat,
                           day1 = day1,
                           dayn = dayn,
                           soilControl = soil.parms,
                           canopyControl = config$pft$canopyControl,
                           phenoControl = phenoParms(),#config$pft$phenoParms,
                           seneControl = config$pft$seneControl,
                           iRhizome = iRhizome,
                           photoControl=config$pft$photoParms)
      
    }
    result.yeari.hourly <- with(tmp.result,
                                data.table(year = yeari, 
                                           doy = DayofYear, 
                                           hour = Hour, ThermalT,
                                           Stem, Leaf, Root, Rhizome, Grain, LAI,
                                           SoilEvaporation, CanopyTrans, 
                                           key = c("year", "doy", "hour")))
    if(yeari == min(years)){
      hourly.results <- result.yeari.hourly
    } else if (yeari > min(years)){
      hourly.results <- rbind(hourly.results, result.yeari.hourly)
    }
  }
  biocro.met.dt <- as.data.table(biocro.met)
  setkeyv(biocro.met.dt, c("year", "doy", "hour"))
  setkeyv(hourly.results, c("year", "doy", "hour"))

  hourly.results <- merge(biocro.met.dt, hourly.results) ## right join
  daily.results <- hourly.results[, list(Stem = max(Stem), Leaf = max(Leaf), Root = max(Root), Rhizome = max(Rhizome), 
Grain = max(Grain), tmax = max(Temp), tmin = min(Temp), tavg = mean(Temp), precip = sum(precip)), by = 'year,doy']

  annual.results <- hourly.results[ ,list(Stem = max(Stem), Leaf = max(Leaf), Root = max(Root), Rhizome = max(Rhizome),
                                                Grain = max(Grain), mat = mean(Temp), map = sum(precip)),
                                          by = "year"]
  return(list(hourly = hourly.results,
              daily = daily.results,
              annually = data.table(lat = lat, lon = lon, annual.results)))
}
