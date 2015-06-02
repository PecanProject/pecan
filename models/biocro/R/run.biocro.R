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
run.biocro <- function(lat, lon, met.nc = met.nc, soil.nc = NULL, 
                       config = config,
                       coppice.interval = 1){
  require(data.table)
  start.date <- ceiling_date(as.POSIXct(config$run$start.date), "day")
  end.date <- floor_date(as.POSIXct(config$run$end.date), "day")
  genus <- config$pft$type$genus

  ## Meteorology
  met <- load.cfmet(met.nc, lat = lat, lon = lon, start.date = start.date, end.date = end.date)
  met.hr <- cfmet.downscale.time(cfmet = met, output.dt = 1)
  biocro.met <- cf2biocro(met.hr)
  

  #if(!is.null(config$pft$soilControl)){
  #  soil.parms <- config$pft$soilControl
  #  soil.parms$iWatCont <- NULL
  #}
  if(!is.null(soil.nc)){
    soil <- get.soil(lat, lon, soil.nc = soil.nc)
    soil.type <- ifelse(soil$usda_class %in% 1:10, soil$usda_class, 10)  
  }
  
  if(!is.null(soil.type)){
    soil.parms <- soilParms(soilType = soil.type)
  } else {
    soil.parms <- soilParms()
  }

  years <- unique(biocro.met$year)
  for(yeari in years){
    yearchar <- as.character(yeari)
    WetDat <- biocro.met[biocro.met$year == yeari, ]
    
    ## TODO PASS day1 from config??
    day1 <- min(WetDat$doy)
    dayn <- max(WetDat$doy)
    ## TODO: start of a distinct function called 'getinitialcondition'
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
        iplant[c("iRhizome", "iRoot", "iStem")] <- last(result.yeari[,list(Rhizome, Root, Stem)])
        if ((yeari - min(years))  %% coppice.interval == 0) { # coppice when remainder = 0
          HarvestedYield  <- round(last(result.yeari$Stem) * 0.95, 2)                
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
        iRhizome <- last(result.yeari[,Rhizome])
        HarvestedYield  <- round(last(result.yeari$Stem) * 0.95, 2)                
      }
      ## run BioGro
      tmp.result <- BioGro(WetDat = WetDat,
                           #day1 = day1, dayn = dayn,
                           soilControl = soil.parms,
                           canopyControl = config$pft$canopyControl,
                           phenoControl = phenoParms(),#config$pft$phenoParms,
                           seneControl = config$pft$seneControl,
                           iRhizome = iRhizome,
                           photoControl=config$pft$photoParms)
      
    }
    result.yeari.hourly <- with(tmp.result,
                                data.table(year = yeari, 
                                           doy = rep(DayofYear, each = 24)[1:length(ThermalT)], 
                                           hour = Hour, ThermalT,
                                           SoilEvaporation, CanopyTrans, 
                                           key = c("year", "doy", "hour")))
    result.yeari.daily <- with(tmp.result, 
                               data.table(year = yeari, 
                                          doy = DayofYear,
                                          Stem, Leaf, Root, Rhizome, Grain, LAI,
                                          key = c("year", "doy")))
    result.yeari <- merge(result.yeari.hourly, result.yeari.daily, by = c("year", "doy"))
    HarvestedYield <- max(result.yeari$Stem)*0.8
    yield.yeari <- data.table(lat = lat, lon = lon, year = yeari, yield = HarvestedYield, runtime = now(),
                              key = "year")
    if(yeari == min(years) | !exists("all.results")){
      all.results <- result.yeari
      yield.annually <- yield.yeari
    } else if (yeari > min(years)){
      all.results <- rbind(all.results, result.yeari)
      yield.annually <- rbind(yield.annually, yield.yeari)
    }
  }    
  setkeyv(biocro.met, c("year", "doy", "hour"))
  setkey(yield.annually, "year")
  setkeyv(all.results, c("year", "doy", "hour"))
  result.hourly <- merge(biocro.met, all.results) ## right join
  
  result.annually <- round(result.hourly[ ,list(Stem = max(Stem), Leaf = max(Leaf), Root = max(Root), Rhizome = max(Rhizome),
                                                Grain = max(Grain), mat = mean(Temp), map = sum(precip)),
                                          by = "year"], 2)[yield.annually]
  return(data.table(lat = lat, lon = lon, result.annually))
}