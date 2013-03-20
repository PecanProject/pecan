#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------#
##' Reads model output and runs sensitivity and ensemble analyses
##'
##' Output is placed in model output directory (settings$run$host$outdir).
##' @name get.results
##' @title Generate model output for PEcAn analyses
##' @export
##' @author Shawn Serbin, David LeBauer, Mike Dietze
##' @param model name of model being used
##' @param settings list, read from settings file (xml) using \code{\link{read.settings}}
##' 
##' @author David LeBauer, Shawn Serbin, Mike Dietze
get.results <- function(pecandir, model) {
  
  ### Load PEcAn sa info
  load(file.path(pecandir, 'samples.Rdata'))
  
  sensitivity.output <- list()
  ensemble.output    <- list()
  
  start.year <- ifelse(is.null(settings$sensitivity.analysis$start.year),
                       NA, settings$sensitivity.analysis$start.year)
  end.year   <- ifelse(is.null(settings$sensitivity.analysis$end.year),
                       NA, settings$sensitivity.analysis$end.year)

  variables <- NULL
  if("sensitivity.analysis" %in% names(settings)){
    if("variable" %in% names(settings$sensitivity.analysis)){
      var <- which(names(settings$sensitivity.analysis) == 'variable')
      for(i in 1:length(var)){
        variables[i] = settings$sensitivity.analysis[[var[i]]]
      }
    }
  }
  print(variables)
  
  if('sensitivity.analysis' %in% names(settings)) {
    
    for(pft.name in names(trait.samples)){
      
      traits <- names(trait.samples[[pft.name]])
      quantiles <- rownames(sa.samples[[pft.name]])
      
      sensitivity.output[[pft.name]] <- read.sa.output(traits = traits,
                                                       quantiles = quantiles,
                                                       pecandir = pecandir,
                                                       outdir = settings$run$host$outdir, 
                                                       pft.name=pft.name,
                                                       start.year=start.year,
                                                       end.year=end.year,
                                                       variables=variables,
                                                       model=model)
    }
  }
  
  if('ensemble' %in% names(settings)) {
    ensemble.output <- read.ensemble.output(settings$ensemble$size,
                                            pecandir = pecandir,
                                            outdir = settings$run$host$outdir, 
                                            start.year=start.year,
                                            end.year=end.year,
                                            variables=variables,
                                            model=model)
  }
  
  save(ensemble.output, sensitivity.output, file = file.path(pecandir, 'output.Rdata'))
}
#==================================================================================================#

