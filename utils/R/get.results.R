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
##' Output is placed in model output directory (settings$modeloutdir).
##' @name get.results
##' @title Generate model output for PEcAn analyses
##' @export
##' @param settings list, read from settings file (xml) using \code{\link{read.settings}}
##' @author David LeBauer, Shawn Serbin, Mike Dietze
get.results <- function(settings) {
  outdir <- settings$outdir
  ### Load PEcAn sa info
  load(file.path(outdir, 'samples.Rdata'))
  
  sensitivity.output <- list()   
  if('sensitivity.analysis' %in% names(settings)) {
    start.year <- ifelse(is.null(settings$sensitivity.analysis$start.year), NA, settings$sensitivity.analysis$start.year)
    end.year   <- ifelse(is.null(settings$sensitivity.analysis$end.year), NA, settings$sensitivity.analysis$end.year)
    variables  <- NULL
    if("variable" %in% names(settings$sensitivity.analysis)){
      variables = settings$sensitivity.analysis[
                 names(settings$sensitivity.analysis) == "variable"]
    }
    for(pft.name in names(trait.samples)){
      
      traits <- names(trait.samples[[pft.name]])
      quantiles <- rownames(sa.samples[[pft.name]])
      
      sensitivity.output[[pft.name]] <- read.sa.output(traits = traits,
                                                       quantiles = quantiles,
                                                       pecandir = outdir,
                                                       outdir = settings$modeloutdir, 
                                                       pft.name=pft.name,
                                                       start.year=start.year,
                                                       end.year=end.year,
                                                       variables=variables)
    }
    save(sensitivity.output, file = file.path(outdir, 'sensitivity.Rdata'))
  }
  
  ensemble.output    <- list()
  if('ensemble' %in% names(settings)) {
    start.year <- ifelse(is.null(settings$ensemble$start.year), NA, settings$ensemble$start.year)
    end.year   <- ifelse(is.null(settings$ensemble$end.year), NA, settings$ensemble$end.year)
    variables  <- NULL
    if("variable" %in% names(settings$ensemble)){
      var <- which(names(settings$ensemble) == 'variable')
      for(i in 1:length(var)){
        variables[i] = settings$ensemble[[var[i]]]
      }
    }
    ensemble.output <- read.ensemble.output(settings$ensemble$size,
                                            pecandir = outdir,
                                            outdir = settings$modeloutdir, 
                                            start.year=start.year,
                                            end.year=end.year,
                                            variables=variables)
    save(ensemble.output, file = file.path(outdir, 'ensemble.Rdata'))
  }
}
#==================================================================================================#

