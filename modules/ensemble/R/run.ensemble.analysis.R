#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------#
##' run ensemble.analysis
##' 
##' @name run.ensemble.analysis
##' @title run ensemble.analysis
##' @return nothing, creates ensemble plots as ensemble.analysis.pdf
##' @export
##' @author David LeBauer, Shawn Serbin
##'
run.ensemble.analysis <- function(){
  if(!exists("settings")){ # temporary hack
                        # waiting on http://stackoverflow.com/q/11005478/199217
    settings <- list(outdir = "/tmp/",
                     pfts = list(pft = list(name = "ebifarm.pavi",
                                   outdir = "/tmp/")),
                     ensemble.analysis = NULL)
  }
  if ('sensitivity.analysis' %in% names(settings)) {
    ### Load parsed model results
    load(paste(settings$outdir, 'output.Rdata', sep=''))
  }

} ### End of function
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.        			
####################################################################################################
