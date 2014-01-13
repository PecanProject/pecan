#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
## overall script for allom

#' @title AllomAve
#' @name  AllomAve
#' @param pfts        pft list from PEcAn settings (if con) OR list of pft spcd's
#' @param components  IDs for allometry components from Jenkins et al 2004 Table 5
#' @param outdir      output directory files are written to
#' @param con         database connection
#' @param field       path(s) to raw data files
#' @param parm        path to allometry equation file (default is Jenkins Table 3)
#' @param ngibbs      number of MCMC iterations (per chain) to run
#' @param nchain      number of MCMC chains
#' @return none
AllomAve <- function(pfts,components=c(6,18,43),outdir,con=NULL,field=NULL,
                     parm=system.file("data/Table3_GTR-NE-319.v2.csv", package = "PEcAn.allometry"),
                     ngibbs=5000,nchain=3){
  ## components:
  ## 6=stem (Bs), 18 = leaf (Bl), 
  ## 40 = height (Ht)
  ## 41 = rooting depth (Rd)
  ## 42 = Rooting volume (Vol)
  ## 43 = Canopy Area
  require(coda)
  
  sel    =  floor(seq(ngibbs*0.25,ngibbs,length=min(ngibbs*0.75,5000)))  

  allom.stats = list()
  
  ########## BAYESIAN VERSION ###############
  for(pft in pfts){  ## loop over PFTs
    allom.stats[[pft$name]] = list()
    
    for(component in components){ 
      print(c(pft,component))
      
      ## load data
      if(!is.null(con)){
        ### If running within PEcAn, grab the data from the database
        allom <- query.allom.data(pft$name,component,con)
      } else {
        allom <- read.allom.data(pft,component,field,parm)        
      }
      
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
           file=paste(outdir,paste("Allom",pft$name,component,"Rdata",sep="."),sep=""))
      
      
      ## Save Posterior information (Pass to update.posterior module)
      if(FALSE){
        
      }
      
      ## Analysis/visualization
      pdf(paste(outdir,paste("Allom",pft$name,component,"MCMC","pdf",sep="."),sep=""))
      plot(mc)
      print(c("DIC",DIC,"pD",pD))
      print(c("DICg",DICg,"pDg",pDg))    
      dev.off()

      allom.stats[[pft$name]][[component]] = summary(mc)      
      
    } ## end component loop
  } ## end PFT loop
  return(allom.stats)
} ## End AllomAvg
