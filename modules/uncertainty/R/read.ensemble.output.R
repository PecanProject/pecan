#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

##' Reads output from model ensemble
##'
##' Reads output for an ensemble of length specified by \code{ensemble.size} and bounded by \code{start.year} and \code{end.year}
##' @title Read ensemble output
##' @return a list of ensemble model output 
##' @param ensemble.size the number of ensemble members run
##' @param outdir directory with model output to use in ensemble analysis
##' @param start.year first year to include in ensemble analysis
##' @param end.year last year to include in ensemble analysis
##' @param variables targe variables for ensemble analysis
##' @param model ecosystem model run
##' @export
#--------------------------------------------------------------------------------------------------#
read.ensemble.output <- function(ensemble.size, outdir, 
                                 start.year, end.year,variables, model){
  if (!exists('runs.samples')) {
    load(file.path(settings$outdir, 'samples.Rdata'))    
  }

  ensemble.output <- list()
  for(row in rownames(runs.samples$ensemble)) {
    run.id <- runs.samples$ensemble[row, 'id']
    ensemble.output[[row]] <- sapply(read.output(run.id, file.path(outdir, run.id), start.year, end.year,variables,model),mean,na.rm=TRUE)
  }
  return(ensemble.output)
}
#==================================================================================================#
