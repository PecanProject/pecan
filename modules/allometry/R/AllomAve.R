#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

#' @title AllomAve
#' @name  AllomAve
#' @aliases AllomAve
#' @param pfts        pft list from PEcAn settings (if con) OR list of pft spcd's
#' \itemize{
#'   \item{"acronym"}{ - USDA species acronyms (see plants.usda.gov), used with FIELD data (vector)}
#'   \item{"spcd"}{ - USFS species codes, use with PARM data (vector)}
#'   \item{"name"}{ - name used to identify output files (single)}
#' }
#' @param components  IDs for allometry components from Jenkins et al 2004 Table 5. Default is stem biomass (6). See data(allom.components)
#' @param outdir      output directory files are written to. Default is getwd()
#' @param con         database connection
#' @param field       path(s) to raw data files
#' @param parm        path to allometry equation file (NULL default loads Jenkins Table 3)
#' @param ngibbs      number of MCMC iterations (per chain) to run
#' @param nchain      number of MCMC chains
#' @return nested list of parameter summary statistics
#' @export
#' @description allometery wrapper function that handles loading and subsetting the data,
#'  fitting the Bayesian models, and generating diagnostic figures. Set up to loop over
#'   multiple PFTs and components. 
#'   Writes raw MCMC and PDF of diagnositcs to file and returns table of summary stats.
#'   
#' @details There are two usages of this function. 
#' When running "online" (connected to the PEcAn database), pass the database connection, con, and the pfts subsection of the PEcAn settings.
#' Wheb running "stand alone" pass the pft list mapping species to species codes and the file paths to the allometry table and field data (optional)
#' 
#' @examples 
#' pfts = list(FAGR = data.frame(spcd=531,name="FAgr",acronym="FAGR"))
#' allom.stats = AllomAve(pfts,ngibbs=500)
#' 
#' @author Michael Dietze
#' 
AllomAve <- function(pfts,components=6,outdir=NULL,con=NULL,field=NULL,
                     parm=system.file("data/Table3_GTR-NE-319.v2.csv", package = "PEcAn.allometry"),
                     ngibbs=5000,nchain=3){
  ## common components:
  ## 6=stem (Bs), 18 = leaf (Bl), 
  ## 40 = height (Ht)
  ## 41 = rooting depth (Rd)
  ## 42 = Rooting volume (Vol)
  ## 43 = Canopy Area
  require(coda)
  
  nested.range <- function(obs){
    w = NULL
    for(i in 1:length(obs)){
      for(j in 1:length(obs[[i]])){
        w = t(apply(cbind(w,t(sapply(obs[[i]][[j]],range))),1,range))
      }
    }
    return(t(w))
  }
    
  if(is.null(outdir)) outdir = getwd()
  print(c("writing output to",outdir))
  
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
      obs <- list()
      for(i in 1:nchain){
        if(component == 40){
          allom.out = allom.BayesFit(allom,ngibbs,"exp")
        } else {
          allom.out = allom.BayesFit(allom,ngibbs)
        }
        mc[[i]] <- as.mcmc(allom.out[["mc"]][sel,])
        obs[[i]] <- allom.out[["obs"]]
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
      outfile = paste(outdir,paste("/Allom",pft$name,component,"Rdata",sep="."),sep="")
      print(c("saving MCMC output to",outfile))
      save(mc,DIC,DICg,pD,pDg,obs,file=outfile)

      allom.stats[[pft$name]][[component]] = summary(mc)      
      
      ## Save Posterior information (Pass to update.posterior module)
      if(FALSE){
        
      }
      
      ## Analysis/visualization
      pdffile = paste(outdir,paste("/Allom",pft$name,component,"MCMC","pdf",sep="."),sep="")
      print(c("saving diagnostic graphs to",pdffile))
      pdf(pdffile)
        ### scatter plot  
        rng = nested.range(obs)
        plot(1,1,type='n',log='xy',xlim=rng[,'x'],ylim=rng[,'y'],#xlim=c(0.1,1000),ylim=c(0.0001,100000),
           xlab="DBH (cm)",ylab="Biomass (kg)")
        #pseudodata
        for(i in 1:nchain){
          for(j in 1:length(obs[[i]])){
            points(obs[[i]][[j]]$x,obs[[i]][[j]]$y,col=j,pch=i)
          }
        }      
        #naive prediction
        dseq = seq(0.1,1000,length=10)  ## diameter sequence
        beta = allom.stats[[pft$name]][[component]]$statistics[,"Mean"]
        y.0  = exp(beta['mu0'] + beta['mu1']*log(dseq))
        y.g  = exp(beta['Bg0'] + beta['Bg1']*log(dseq))      
        lines(dseq,y.0,lwd=2,col=1)
        lines(dseq,y.g,lwd=2,col=2)
        legend("topleft",legend=c("Hier","global"),lwd=2,col=1:2)
      
        ### MCMC diagnostics
        plot(mc)
      dev.off()
      
      ## DIC statistics
      print(c("DIC",DIC,"pD",pD))
      print(c("DICg",DICg,"pDg",pDg))    
      
    } ## end component loop
  } ## end PFT loop
  return(allom.stats)
} ## End AllomAvg

