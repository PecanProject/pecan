#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------------#
##' 
##' Start biocro model runs on local
##' @title Start run of biocro model
##' @param runid the id of the run (folder in runs) to execute
##' @export
##' @author Rob Kooper, David LeBauer, Deepak Jaiswal
start.runs.BIOCRO <- function(runid) {

    hostname <- system("hostname", intern = TRUE)
  
  require("BioCro")
  if(settings$run$host$name == "localhost"){
      settings$run$host$name <- hostname
  } else {
      logger.error("BioCro module only configured to run locally")
  }

  rundir <- file.path(settings$run$host$rundir, as.character(runid))
  outdir <- file.path(settings$run$host$outdir, as.character(runid))

  # run model
  
  weather <- get.ncepmet() ## 
  lat <- as.numeric(settings$run$site$lat)
  W <- data.table(weachNEW(weather, lati = lat, ts = 1, 
                                  temp.units="Celsius", rh.units="fraction", 
                                  ws.units="mph", pp.units="in"))

  years <- unique(W$year)

  # run model
  
  config <- xmlToList(xmlParse(file.path(rundir, "config.xml")))
  pp.config <- config$pft$photoParms
  pp <- lapply(photoParms(vmax=pp.config$vmax, b0=pp.config$b0, b1 = pp.config$b1,Rd=pp.config$Rd), as.numeric)
  cc <- canopyParms(Sp = as.numeric(config$pft$canopyControl$Sp))

  genus <- config$pft$genus


  if(!(genus %in% c("Saccharum", "Salix", "Miscanthus"))) {
      logger.error("genus", genus, "not supported by PEcAn.BIOCRO module")
  }

  out <- NULL ## could be pre-allocated for speed
  result <- NULL

  for(yeari in years){
      WetDat <- W[W$year == yeari,]
      day1 <- min(WetDat$doy)
      dayn <- max(WetDat$doy)
      if(genus == "Saccharum"){
          result <- caneGro(WetDat = WetDat, photoControl=pp, canopyControl=cc)
          result[["Grain"]] <- result[["Rhizome"]] <- rep(0, length(result$Hour))
      } else if (genus == "Salix") {
          if(is.null(result)){
              iplant <- iwillowParms(iRhizome=1.0, iStem=1.0, iLeaf=0.0,
                                     iRoot=1.0, ifrRhizome=0.01, ifrStem=0.01,
                                     ifrLeaf = 0.0, ifrRoot = 0.0)
          } else if(!is.null(result)){
              r <- last(result[, c("Rhizome", "Stem", "Root")])
              iplant$iRhizome <- r$Rhizome
              iplant$iStem <- r$Stem
              iplant$iRoot <- r$Root
          }
          result <- willowGro(WetDat = WetDat, photoControl=pp, canopyControl=cc,
                              day1 = day1, dayn = dayn)
      } else if (genus == "Miscanthus") {
          result <- BioGro(WetDat = WetDat, photoControl = pp, canopyControl = cc)
      }
  
      result <- with(result,
                 data.table(Year = yeari, DayofYear, Hour, ThermalT,
                            Stem, Leaf, Root, Rhizome, Grain, LAI, SoilEvaporation, CanopyTrans))
      if(is.null(out)) {
          out <- result
      } else {
          out <- rbind(out, result)
      }
  }
  result <- out
  if(is.null(result)) logger.error("no output from BioCro")
  
  write.csv(result, file=file.path(outdir, "result.csv"))

  save(result, config, file = file.path(outdir, "result.RData"))

  file.copy(file.path(rundir, "README.txt"), file.path(outdir, "README.txt"))
}
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.          		
####################################################################################################
