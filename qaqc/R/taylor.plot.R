#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

##' plot taylor diagram for benchmark sites
##' @title Taylor Diagram
##' @param runid a numeric vector with the id(s) of one or more runs (folder in runs) to plot
new.taylor <- function(obs,dataset,runid){
  for(run in runid){
    taylor.diagram(obs,dataset$paste("model",runid),pos.cor=FALSE,add=TRUE)
  }
}