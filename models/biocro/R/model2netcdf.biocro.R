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
##' @name model2netcdf.c4photo
##' @title Function to convert c4photo model output to standard netCDF format
##' @param outdir Location of c4photo model output
##' @param run.id Name of c4photo model output file.
##' 
##' @export
##' @author Shawn Serbin, Michael Dietze, David LeBauer
model2netcdf.biocro <- function(outdir, run.id) {
  
  require(ncdf4)
  
  ### Read in model output in c4photo format
  outfile <- paste(outdir,"/",run.id,"/",run.id,".Rdata",sep="")
  load(outfile)
  biocro.output <- result 
  output <- list()

# Focus on Stem Biomass only for now
#   output[["Gs"]]    <- c4photo.output$Gs
#   output[["Assim"]] <- c4photo.output$Assim
  output[["Stem"]]    <- c4photo.output$Stem
  
  var <- list()
  null.dim <- dim.def.ncdf("NULL", "", 1) 
#   var[["Gs"]]    <- var.def.ncdf("Gs", "mmol m-2 s-1", null.dim, -999,
#                                 "Stomatal Conductance")
#   var[["Assim"]] <- var.def.ncdf("Assim", "umol m-2 s-1", null.dim, -999,
#                                 "Net Assimilation")
  var[["Stem"]] <- var.def.ncdf("Stem", "Mg ha-1", null.dim, -999,
                              "Stem Biomass")
  
  
  
  ##******************** Declare netCDF variables ********************#
  nc <- create.ncdf(paste(outdir,"/",run.id,"/",run.id,".nc",sep=""),var)
  
  ## Output netCDF data
  for(i in 1:length(var)){
    put.var.ncdf(nc,var[[i]],output[[i]])  
  }
  close.ncdf(nc) 
}

####################################################################################################
### EOF.  End of R script file.              
####################################################################################################
