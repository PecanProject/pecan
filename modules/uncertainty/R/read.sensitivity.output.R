#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------#
##' Reads output of sensitivity analysis runs
##'
##' 
##' @title Read Sensitivity Analysis output 
##' @return dataframe with one col per quantile analysed and one row per trait,
##'  each cell is a list of AGB over time
##' @param traits model parameters included in the sensitivity analysis
##' @param quantiles quantiles selected for sensitivity analysis
##' @param outdir directory with model output to use in sensitivity analysis
##' @param pft.name name of PFT used in sensitivity analysis (Optional)
##' @param start.year first year to include in sensitivity analysis 
##' @param end.year last year to include in sensitivity analysis
##' @param read.output model specific read.output function
##' @export
#--------------------------------------------------------------------------------------------------#
read.sa.output <- function(traits, quantiles, outdir, pft.name='', 
                           start.year, end.year, variables, model){
  
  if (!exists('runs.samples')) {
    load(file.path(settings$outdir, 'samples.Rdata'))    
  }

  sa.output <- matrix(nrow = length(quantiles),
                      ncol = length(traits),
                      dimnames = list(quantiles, traits))
  for(trait in traits){
    for(quantile in quantiles){
      run.id <- runs.samples$sa[[pft.name]][quantile, trait]
      sa.output[quantile, trait] <- sapply(read.output(run.id, file.path(outdir, run.id),
                                                       start.year, end.year,
                                                       variables, model),
                                           mean, na.rm=TRUE)
    } ## end loop over quantiles
  } ## end loop over traits
  sa.output <- as.data.frame(sa.output)
  return(sa.output)
}
#==================================================================================================#
