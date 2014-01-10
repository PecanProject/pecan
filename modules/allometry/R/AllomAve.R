#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
## overall script for allom

#'
#' 
#'   

AllomAve <- function(pfts,components=c(6,18,43),con=NULL,field=NULL,parm=NULL,ngibbs=5000,nchain=3){
  ## components:
  ## 6=stem (Bs), 18 = leaf (Bl), 
  ## 40 = height (Ht)
  ## 41 = rooting depth (Rd)
  ## 42 = Rooting volume (Vol)
  ## 43 = Canopy Area
  
  sel    =  floor(seq(ngibbs*0.25,ngibbs,length=min(ngibbs*0.75,5000)))  
  
  ########## BAYESIAN VERSION ###############
  for(pft in pfts){  ## loop over PFTs
    
    for(component in components{ 
      print(c(pft,component))
      
      ## load data
      if(!is.null(con)){
        ### If running within PEcAn, grab the data from the database
        allom <- query.allom.data(pft$name,component,con)
      } else {
        ### Otherwise, read csv files directly
        allom <- list(parm=NULL,field=NULL)
        if(!is.null(field)){
          allom$field = list()        
          for(i in 1:length(field)){
            allom$field[[i]] = read.csv(field[i])
          }
        }
        if(!is.null(parm)){
          allom$parm = read.csv(parm)
          }
        }
        ### Need to add code that does the PFT selection
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
           file=paste(pft$outdir,paste("Allom",pft$name,component,"Rdata",sep="."),sep=""))
      
      
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
} ## End AllomAvg
