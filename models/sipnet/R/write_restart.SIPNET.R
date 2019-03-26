#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

##' @title write_restart.SIPNET
##' @name  write_restart.SIPNET
##' @author Ann Raiho \email{araiho@@nd.edu}
##'
##' @param outdir output directory
##' @param runid run ID
##' @param start.time start date and time for each SDA ensemble
##' @param stop.time stop date and time for each SDA ensemble
##' @param settings PEcAn settings object
##' @param new.state analysis state vector
##' @param RENAME flag to either rename output file or not
##' @param new.params list of parameters to convert between different states 
##' @param inputs list of model inputs to use in write.configs.SIPNET
##'
##' @description Write restart files for SIPNET
##'
##' @return NONE
##' @export
write_restart.SIPNET <- function(outdir, runid, start.time, stop.time, settings, new.state,
                                 RENAME = TRUE, new.params = FALSE, inputs) {
  
  rundir <- settings$host$rundir
  variables <- colnames(new.state)
  # values that will be used for updating other states deterministically depending on the SDA states
  IC_extra <- data.frame(t(new.params$restart))
  
  if (RENAME) {
    file.rename(file.path(outdir, runid, "sipnet.out"),
                file.path(outdir, runid, paste0("sipnet.", as.Date(start.time), ".out")))
    system(paste("rm", file.path(rundir, runid, "sipnet.clim")))
  } else {
    print(paste("Files not renamed -- Need to rerun year", start.time, "before next time step"))
  }
  
  settings$run$start.date <- start.time
  settings$run$end.date <- stop.time
  
  ## Converting to sipnet units
  prior.sla <- new.params[[which(!names(new.params) %in% c("soil", "soil_SDA", "restart"))[1]]]$SLA
  unit.conv <- 2 * (10000 / 1) * (1 / 1000) * (3.154 * 10^7)  # kgC/m2/s -> Mg/ha/yr
  
  analysis.save <- list()
  
  if ("NPP" %in% variables) {
    analysis.save[[length(analysis.save) + 1]] <- udunits2::ud.convert(new.state$NPP, "kg/m^2/s", "Mg/ha/yr")  #*unit.conv -> Mg/ha/yr
    names(analysis.save[[length(analysis.save)]]) <- c("NPP")
  }
  
  if ("AbvGrndWood" %in% variables) {
    AbvGrndWood <- udunits2::ud.convert(new.state$AbvGrndWood,  "Mg/ha", "g/m^2")
    analysis.save[[length(analysis.save) + 1]] <- AbvGrndWood 
    names(analysis.save[[length(analysis.save)]]) <- c("AbvGrndWood")
    
    analysis.save[[length(analysis.save) + 1]] <- IC_extra$abvGrndWoodFrac
    names(analysis.save[[length(analysis.save)]]) <- c("abvGrndWoodFrac")
    
    analysis.save[[length(analysis.save) + 1]] <- IC_extra$coarseRootFrac 
    names(analysis.save[[length(analysis.save)]]) <- c("coarseRootFrac")
    
    analysis.save[[length(analysis.save) + 1]] <- IC_extra$fineRootFrac 
    names(analysis.save[[length(analysis.save)]]) <- c("fineRootFrac")
  }
  
  if ("LeafC" %in% variables) {
    analysis.save[[length(analysis.save) + 1]] <- new.state$LeafC * prior.sla * 2  ## kgC/m2*m2/kg*2kg/kgC -> m2/m2
    if (new.state$LeafC < 0) analysis.save[[length(analysis.save)]] <- 0
    names(analysis.save[[length(analysis.save)]]) <- c("lai")
  }
  
  if ("Litter" %in% variables) {
    analysis.save[[length(analysis.save) + 1]] <- udunits2::ud.convert(new.state$Litter, 'kg m-2', 'g m-2') # kgC/m2 -> gC/m2
    if (new.state$Litter < 0) analysis.save[[length(analysis.save)]] <- 0
    names(analysis.save[[length(analysis.save)]]) <- c("litter")
  }
  
  if ("TotSoilCarb" %in% variables) {
    analysis.save[[length(analysis.save) + 1]] <- udunits2::ud.convert(new.state$TotSoilCarb, 'kg m-2', 'g m-2') # kgC/m2 -> gC/m2
    if (new.state$TotSoilCarb < 0) analysis.save[[length(analysis.save)]] <- 0
    names(analysis.save[[length(analysis.save)]]) <- c("soil")
  }
  
  if ("SoilMoistFrac" %in% variables) {
    analysis.save[[length(analysis.save) + 1]] <- new.state$SoilMoistFrac  ## unitless
    if (new.state$SoilMoistFrac < 0 | new.state$SoilMoistFrac > 1) analysis.save[[length(analysis.save)]] <- 0.5
    names(analysis.save[[length(analysis.save)]]) <- c("litterWFrac")
    
    analysis.save[[length(analysis.save) + 1]] <- new.state$SoilMoistFrac  ## unitless
    if (new.state$SoilMoistFrac < 0 | new.state$SoilMoistFrac > 1) analysis.save[[length(analysis.save)]] <- 0.5
    names(analysis.save[[length(analysis.save)]]) <- c("soilWFrac")
  }
  
  if ("SWE" %in% variables) {
    analysis.save[[length(analysis.save) + 1]] <- new.state$SWE/10  
    if (new.state$SWE < 0) analysis.save[[length(analysis.save)]] <- 0
    names(analysis.save[[length(analysis.save)]]) <- c("snow")
  }

  
  if (!is.null(analysis.save) & length(analysis.save)>0){
    analysis.save.mat <- data.frame(matrix(unlist(analysis.save, use.names = TRUE), nrow = 1))
    colnames(analysis.save.mat) <- names(unlist(analysis.save))
  }else{
    analysis.save.mat<-NULL
  }

  
  do.call(write.config.SIPNET, args = list(defaults = NULL,
                                           trait.values = new.params,
                                           settings = settings,
                                           run.id = runid,
                                           inputs = inputs,
                                           IC = analysis.save.mat))
  print(runid)
} # write_restart.SIPNET
