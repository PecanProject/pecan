#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
## overall script for allom

## load pecan settings
if(interactive()){
 settings.file = "/home/mdietze/pecan/tests/settings.bartlett.xml"
} else {
  settings.file <- Sys.getenv("PECANSETTINGS")
}
library(XML)
settings.xml <- xmlParse(settings.file)
settings <- xmlToList(settings.xml)
if(!is.null(settings$Rlib)){ .libPaths(settings$Rlib)} 
require(PECAn)

## libraries & database connection
library(mvtnorm)
library(MCMCpack)
haveMPI <- require(Rmpi)
library(RMySQL)
dvr <- dbDriver("MySQL")
con <- query.base.con(dbname   = settings$database$name,
                                    password = settings$database$passwd,
                                    username = settings$database$userid,
                                    host     = settings$database$host)

## mcmc settings
ngibbs = nu(settings$meta.analysis$iter)
nchain = 3
sel    =  floor(seq(ngibbs*0.25,ngibbs,length=min(ngibbs*0.75,5000)))

########## BAYESIAN VERSION ###############
for(pft in settings$pfts){  ## loop over PFTs

  for(component in c(6,18,43)){ ## 6=stem (Bs), 18 = leaf (Bl), 
                                   ## 40 = height (Ht)
                                   ## 41 = rooting depth (Rd)
                                   ## 42 = Rooting volume (Vol)
                                   ## 43 = Canopy Area
    print(c(pft,component))
    
    ## load data
    allom <- query.allom.data(pft$name,component,con)

    if(is.null(allom) | (is.null(allom$parm) & is.null(allom$field))){
      next
    }
    
    mc <- list()
    for(i in 1:nchain){
      if(component == 40){
        mc[[i]] <- as.mcmc(allom.BayesFit(allom,ngibbs)[sel,],"exp")
      } else {
        mc[[i]] <- as.mcmc(allom.BayesFit(allom,ngibbs)[sel,])
      }
    }
    mc <- as.mcmc.list(mc)

    ## Model Selection
    D = as.array(mc)[,'D',]
    pD = mean(D) - min(D)
    DIC = mean(D) + pD
    
    Dg = as.array(mc)[,'Dg',]
    pDg = mean(Dg) - min(Dg)
    DICg = mean(Dg) + pDg
    
    ## Save MCMC objects (Pass to MCMC diagnostics module)
    save(mc,DIC,DICg,pD,pDg,
         file=paste(pft$outdir,paste("Allom",pft$name,component,"RData",sep="."),sep=""))
        
    
    ## Save Posterior information (Pass to update.posterior module)
    if(FALSE){

    }
    
    ## Analysis/visualization
    pdf(paste(pft$outdir,paste("Allom",pft$name,component,"MCMC","pdf",sep="."),sep=""))
    plot(mc)
    summary(mc)
    print(c("DIC",DIC,"pD",pD))
    print(c("DICg",DICg,"pDg",pDg))    
    dev.off()
    
  } ## end component loop
} ## end PFT loop
