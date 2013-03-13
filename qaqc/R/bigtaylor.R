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
<<<<<<< HEAD
new.taylor <- function(dataset,runid,siteid,yrid){
  attach(dataset)
  require(plotrix)
  for(yr in yrid){  
    for(run in runid){
     for (si in siteid){
      if (run==runid[1]&&si==siteid[1]&&yr==yrid[1]){
        taylor.diagram(obs[site %in% si][date %in% yr],get(paste("model",run,sep=""))[site %in% si][date %in% yr],pos.cor=FALSE)
        R <- cor(obs[site %in% si][date %in% yr],get(paste("model",run,sep=""))[site %in% si][date %in% yr],use="pairwise")
        sd.f <- sd(get(paste("model",run,sep=""))[site %in% si][date %in% yr])
        lab=paste(paste("model",run,sep=""), paste("site",si,sep=""), paste("year",yr,sep=""))
        text(sd.f * R, sd.f * sin(acos(R)), labels=lab,pos=3)
      }
      else {
        taylor.diagram(obs[site %in% si][date %in% yr],get(paste("model",run,sep=""))[site %in% si][date %in% yr],pos.cor=FALSE,add=TRUE)
        R <- cor(obs[site %in% si][date %in% yr],get(paste("model",run,sep=""))[site %in% si][date %in% yr],use="pairwise")
        sd.f <- sd(get(paste("model",run,sep=""))[site %in% si][date %in% yr])
        lab=paste(paste("model",run,sep=""), paste("site",si,sep=""), paste("year",yr,sep=""))
        text(sd.f * R, sd.f * sin(acos(R)), labels=lab,pos=3)
      }
     }
    }
  }
}


#===========================================================================#

new.taylor <- function(dataset,modelid,runid,siteid,yrid){
  attach(dataset)
  require(plotrix)
  for(yr in yrid){
    for(md in modelid){
      for(si in siteid){
        for (run in runid[which(modelid==md)]){
          if (md==modelid[1]&&run==runid[1]&&si==siteid[1]&&yr==yrid[1]){
            taylor.diagram(obs[site %in% si][date %in% yr],get(paste(md,run,sep=""))[site %in% si][date %in% yr],pos.cor=FALSE)
            R <- cor(obs[site %in% si][date %in% yr],get(paste(md,run,sep=""))[site %in% si][date %in% yr],use="pairwise")
            sd.f <- sd(get(paste(md,run,sep=""))[site %in% si][date %in% yr])
          }
          else {
            taylor.diagram(obs[site %in% si][date %in% yr],get(paste(md,run,sep=""))[site %in% si][date %in% yr],pos.cor=FALSE,add=TRUE)
            R <- cor(obs[site %in% si][date %in% yr],get(paste(md,run,sep=""))[site %in% si][date %in% yr],use="pairwise")
            sd.f <- sd(get(paste(md,run,sep=""))[site %in% si][date %in% yr])
          }
        }
        lab=paste(paste(md,run,sep=""), paste("site",si,sep=""), paste("year",yr,sep=""))
        text(sd.f * R, sd.f * sin(acos(R)), labels=lab,pos=3)
      }
    }
  }
}
new.taylor(testdata,c("SIPNET","ED"),c(2,2),1,c(2002,2003))


