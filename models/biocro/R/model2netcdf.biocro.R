#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------#
##' Convert BioCro output to netCDF
##'
##' Modified from on model2netcdf.sipnet and model2netcdf.ED2 by
##' Shawn Serbin and Mike Dietze
##' @name model2netcdf.biocro
##' @title Function to convert biocro model output to standard netCDF format
##' @param outdir Location of biocro model output
##' @param run.id Name of biocro model output file.
##' @export
##' @author David LeBauer, Deepak Jaiswal
model2netcdf.BIOCRO <- function(outdir) {
  
  # TODO : this is not working, only saves 1 year

  require(ncdf4)
  require(lubridate)
  ### Read in model output in biocro format
  outfile <- file.path(outdir, "result.Rdata")
  load(outfile)

  ## Focus on Stem Biomass only for now
  #output[["Stem"]]    <- biocro.output$Stem

  # TODO : this is bogus probably!!!
  # compute GPP
  result[["GPP"]] <- result[["CanopyAssim"]]
  
  t <- ncdim_def("time", "seconds", 3600)

  var <- list()
  var[["Stem"]]    <- ncvar_def("Stem", "Mg ha-1", t, -999, "Stem Biomass")
  var[["GPP"]]    <- ncvar_def("GPP", "", t, -999, "Canopy Assimilation")

  ## var[["Leaf"]]    <- ncvar_def("Leaf", "Mg ha-1", t, -999,
  ##                               "Leaf Biomass")
  ## var[["Root"]]    <- ncvar_def("Root", "Mg ha-1", t, -999,
  ##                               "Root Biomass")
  ## var[["Rhizome"]] <- ncvar_def("Rhizome", "Mg ha-1", t, -999,
  ##                              "Rhizome Biomass")
  ## var[["LAI"]]     <- ncvar_def("LAI", "m2/m2", t, -999,
  ##                              "Leaf Area Index")
  ## var[["GPP"]]     <- ncvar_def("GPP", "", t, -999,
  ##                                "Canopy Assimilation")
  ## var[["Transpiration"]]     <- ncvar_def("Assim", "?", t, -999,
  ##                                        "Canopy Transpiration")
  
  ##******************** Declare netCDF variables ********************#
  start_year <- format(as.Date(settings$run$start.date), "%Y")
  end_year <- format(as.Date(settings$run$end.date), "%Y")

  nc.outfile <- file.path(outdir, paste0(start_year, ".nc"))
  nc <- nc_create(filename = nc.outfile, var)
  
  ## Output netCDF data
  for(name in names(var)) {
    ncatt_put(nc, var[[name]], name, result[[name]])  
  }
  nc_close(nc) 
}

####################################################################################################
### EOF.  End of R script file.              
####################################################################################################
