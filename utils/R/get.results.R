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
##' @name get.results
##' @title Generate model output for PEcAn analyses
##'
##' @import PEcAn.utils
##' @export
##'
get.results <- function(model){

  variables <- "GPP"
  model <- settings$model$name
  
  ### OLD CODE, SLIGHTYL MODIFIED, THAT NEEDS TO BE UPDATED. previously names read.output.ed and was in the 
  ### scripts folder.  SPS

  ### Load PEcAn sa info
  load('samples.Rdata')
  
  sensitivity.output <- list()
  ensemble.output    <- list()
  
  start.year <- ifelse(is.null(settings$sensitivity.analysis$start.year),
                       NA, settings$sensitivity.analysis$start.year)
  end.year   <- ifelse(is.null(settings$sensitivity.analysis$end.year),
                       NA, settings$sensitivity.analysis$end.year)

  if("sensitivity.analysis" %in% names(settings)){
    if("variable" %in% names(settings$sensitivity.analysis)){
      var = which(names(settings$sensitivity.analysis) == 'variable')
      for(i in 1:length(var)){
        variables[i] = settings$sensitivity.analysis[[var[i]]]
      }
    }
  }
  print(variables)
  
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
                                                       start.year=start.year,
                                                       end.year=end.year,
                                                       variables=variables,
                                                       model=model)
      save(sensitivity.output, file = 'output.Rdata')
      
    }
  }
  
  if('ensemble' %in% names(settings)) {
    ensemble.output <- read.ensemble.output(settings$ensemble$size,
                                            outdir = settings$outdir, 
                                            start.year=start.year,
                                            end.year=end.year,
                                            variables=variables,
                                            model=model)
    save(ensemble.output, file = 'output.Rdata')
  }
  
  if(all(c('ensemble', 'sensitivity.analysis') %in% names(settings))) {
    save(ensemble.output, sensitivity.output, file = 'output.Rdata')
  }
}
#==================================================================================================#

