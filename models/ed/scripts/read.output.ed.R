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
##' @name read.output.ed
##' @title Extract ED2 model output for analysis
##'
##' @import PEcAn.utils
##' @export
read.output.ed <- function(){
  ### OLD CODE THAT NEEDS TO BE UPDATED. SPS
  ### Rkelly: For one, need to fix ensemble.Rdata and sensitivity.Rdata filenames to include ensemble id. 
  sensitivity.output <- list()
  ensemble.output    <- list()
  
  start.year <- ifelse(is.null(settings$sensitivity.analysis$start.year),
                       NA, settings$sensitivity.analysis$start.year)
  end.year   <- ifelse(is.null(settings$sensitivity.analysis$end.year),
                       NA, settings$sensitivity.analysis$end.year)
  
  if('sensitivity.analysis' %in% names(settings)) {
    
    for(pft.name in names(trait.samples)){
      
      traits <- names(trait.samples[[pft.name]])
      quantiles.str <- rownames(sa.samples[[pft.name]])
      quantiles.str <- quantiles.str[which(quantiles.str != '50')]
      quantiles <- as.numeric(quantiles.str)/100
      
      sensitivity.output[[pft.name]] <- read.sa.output(traits,
                                                       quantiles,
                                                       outdir = getwd(), 
                                                       pft.name=pft.name,
                                                       start.year,
                                                       end.year)      
    }
    save(sensitivity.output, file = file.path(outdir, 'sensitivity.Rdata'))
  }
  
  if('ensemble' %in% names(settings)) {
    ensemble.output <- read.ensemble.output(settings$ensemble$size,
                                            outdir = getwd(), 
                                            start.year,
                                            end.year)
    save(ensemble.output, file = file.path(outdir, 'ensemble.Rdata'))
  }
}
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.          		
####################################################################################################