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
##' @param dataset
##' @param runid a numeric vector with the id(s) of one or more runs (folder in runs) to plot
##' @param siteid
new.taylor <- function(dataset, runid, siteid){
  attach(dataset)
  for(run in runid){
    for (si in siteid){
      if (run==runid[1]&&si==siteid[1]){
        taylor.diagram(obs[site %in% si],get(paste("model",run,sep=""))[site %in% si],pos.cor=FALSE)
        R <- cor(obs[site %in% si],get(paste("model",run,sep=""))[site %in% si],use="pairwise")
        sd.f <- sd(get(paste("model",run,sep=""))[site %in% si])
        lab=paste(paste("model",run,sep=""), paste("site",si,sep=""))
        text(sd.f * R, sd.f * sin(acos(R)), labels=lab,pos=3)
      }
      else {
        taylor.diagram(obs[site %in% si],get(paste("model",run,sep=""))[site %in% si],pos.cor=FALSE,add=TRUE)
        R <- cor(obs[site %in% si],get(paste("model",run,sep=""))[site %in% si],use="pairwise")
        sd.f <- sd(get(paste("model",run,sep=""))[site %in% si])
        lab=paste(paste("model",run,sep=""), paste("site",si,sep=""))
        text(sd.f * R, sd.f * sin(acos(R)), labels=lab,pos=3)
      }
    }
    }

}