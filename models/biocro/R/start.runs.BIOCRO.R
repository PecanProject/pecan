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
##' @export start.runs.BIOCRO
##' @author Rob Kooper, David LeBauer, Deepak Jaiswal
start.runs.BIOCRO <- function(runid) {

    hostname <- system("hostname", intern = TRUE)
  
    require("BioCro")
    if(!(settings$run$host$name == hostname)){
        if(settings$run$host$name == "localhost"){
            settings$run$host$name <- hostname
        } else {
            logger.error("BioCro module only configured to run locally")
        }
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
      logger.severe("genus", genus, "not supported by PEcAn.BIOCRO module")
  }
    
    result <- list()
    
  for(yeari in years){

      yearchar <- as.character(yeari)
      WetDat <- W[W$year == yeari,]
      day1 <- min(WetDat$doy)
      dayn <- max(WetDat$doy)
      
      if(genus == "Saccharum"){
          result[[yearchar]] <- caneGro(WetDat = WetDat, photoControl=pp, canopyControl=cc)
          result[[yearchar]][["Grain"]] <- result[[yearchar]][["Rhizome"]] <- rep(0, length(result$Hour))
      } else if (genus == "Salix") {
          if(yeari == min(years)){
              iplant <- iwillowParms(iRhizome=1.0, iStem=1.0, iLeaf=0.0,
                                     iRoot=1.0, ifrRhizome=0.01, ifrStem=0.01,
                                     ifrLeaf = 0.0, ifrRoot = 0.0)
          } else if(yeari > min(years)){
              iplant$iRhizome <- last(result[[as.character(yeari-1)]]$Rhizome)
              iplant$iStem <- last(result[[as.character(yeari-1)]]$Stem)
              iplant$iRoot <- last(result[[as.character(yeari-1)]]$Root)
          }
          result[[yearchar]] <- willowGro(WetDat = WetDat, photoControl=pp,
                                       canopyControl=cc, day1 = day1, dayn = dayn)
      } else if (genus == "Miscanthus") {
          result[[yearchar]] <- BioGro(WetDat = WetDat, photoControl = pp, canopyControl = cc)
      }

      result.yeari <- with(result[[yearchar]],
                           data.table(Year = yeari, DayofYear, Hour, ThermalT,
                                      Stem, Leaf, Root, Rhizome, Grain, LAI,
                                      SoilEvaporation, CanopyTrans))
      if(yeari == min(years)){
          resultDT <- result.yeari
      } else if (yeari > min(years)){
          resultDT <- rbind(resultDT, result.yeari)
      }
  }
    
    write.csv(resultDT, file=file.path(outdir, "result.csv"))
    
#    pdf(file.path(outdir, "result.pdf"))
#    lapply(result, plot)
#    dev.off()
    
    save(resultDT, config, file = file.path(outdir, "result.RData"))
    
    file.copy(file.path(rundir, "README.txt"), file.path(outdir, "README.txt"), overwrite = TRUE)
}
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.          		
####################################################################################################
