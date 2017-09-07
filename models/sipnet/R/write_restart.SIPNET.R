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
##' @param outdir         output directory
##' @param runid           run ID
##' @param time            year that is being read
##' @param settings        PEcAn settings object
##' @param new.state       analysis state vector
##' @param RENAME          flag to either rename output file or not
##' @param variables
##' @param sample_parameters
##' @param trait.values
##' @param met
##'
##' @description Write restart files for SIPNET
##'
##' @return NONE
##' @export
write_restart.SIPNET <- function(outdir, runid, start.time, stop.time, settings, new.state,
                                 RENAME = TRUE, new.params = FALSE, inputs) {

  rundir <- settings$host$rundir
  variables <- colnames(new.state)

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
  prior.sla <- new.params[[which(names(new.params) != "soil")[1]]]$SLA[1]
  unit.conv <- 2 * (10000 / 1) * (1 / 1000) * (3.154 * 10^7)  # kgC/m2/s -> Mg/ha/yr

  analysis.save <- list()

  if ("NPP" %in% variables) {
    analysis.save[[1]] <- udunits2::ud.convert(new.state$NPP, "kg/m^2/s", "Mg/ha/yr")  #*unit.conv -> Mg/ha/yr
    names(analysis.save[[1]]) <- c("NPP")
  }

  if ("AbvGrndWood" %in% variables) {
    analysis.save[[2]] <- udunits2::ud.convert(new.state$AbvGrndWood, "kg/m^2", "g/m^2")#no (1-.2-.2) because that's on sipnet side
    names(analysis.save[[2]]) <- c("plantWood")
  }

  if ("LeafC" %in% variables) {
    analysis.save[[3]] <- new.state$LeafC * prior.sla * 2  ## kgC/m2*m2/kg*2kg/kgC -> m2/m2
    if (new.state$LeafC < 0)
      analysis.save[[3]] <- 0
    names(analysis.save[[3]]) <- c("lai")
  }

  if ("Litter" %in% variables) {
    analysis.save[[4]] <- udunits2::ud.convert(new.state$Litter, 'kg m-2', 'g m-2') # kgC/m2 -> gC/m2
    if (new.state$Litter < 0)
      analysis.save[[4]] <- 0
    names(analysis.save[[4]]) <- c("litter")
  }

  if ("TotSoilCarb" %in% variables) {
    analysis.save[[5]] <- udunits2::ud.convert(new.state$TotSoilCarb, 'kg m-2', 'g m-2') # kgC/m2 -> gC/m2
    names(analysis.save[[5]]) <- c("soil")
  }

  if ("SoilMoistFrac" %in% variables) {
    analysis.save[[6]] <- new.state$SoilMoistFrac  ## unitless
    if (new.state$SoilMoistFrac < 0 | new.state$SoilMoistFrac > 1)
      analysis.save[[6]] <- 0.5
    names(analysis.save[[6]]) <- c("litterWFrac")

    analysis.save[[7]] <- new.state$SoilMoistFrac  ## unitless
    if (new.state$SoilMoistFrac < 0 | new.state$SoilMoistFrac > 1)
      analysis.save[[7]] <- 0.5
    names(analysis.save[[7]]) <- c("soilWFrac")
  }

  if ("SWE" %in% variables) {
    analysis.save[[8]] <- new.state$SWE  ## unitless
    if (new.state$SWE < 0)
      new.state$SWE <- 0
    names(analysis.save[[8]]) <- c("snow")
  }

  analysis.save.mat <- data.frame(matrix(unlist(analysis.save, use.names = TRUE), nrow = 1))
  colnames(analysis.save.mat) <- names(unlist(analysis.save))

  do.call(write.config.SIPNET, args = list(defaults = NULL,
                                           trait.values = new.params,
                                           settings = settings,
                                           run.id = runid,
                                           inputs = inputs,
                                           IC = analysis.save.mat))
  print(runid)
} # write_restart.SIPNET
