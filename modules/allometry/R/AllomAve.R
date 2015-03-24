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
#' If the latter, the names within the list are used to identify PFTs
#' \itemize{
#'   \item{"acronym"}{ - USDA species acronyms (see plants.usda.gov), used with FIELD data (vector)}
#'   \item{"spcd"}{ - USFS species codes, use with PARM data (vector)}
#' }
#' @param components  IDs for allometry components from Jenkins et al 2004 Table 5. Default is stem biomass (6). See data(allom.components)
#' @param outdir      output directory files are written to. Default is getwd()
#' @param con         database connection
#' @param field       path(s) to raw data files
#' @param parm        path to allometry equation file (NULL default loads Jenkins Table 3)
#' @param ngibbs      number of MCMC iterations (per chain) to run
#' @param nchain      number of MCMC chains
#' @param dmin        minimum dbh of interest
#' @param dmax        maximum dbh of interest
#' @return nested list of parameter summary statistics
#' @export
#' @description allometery wrapper function that handles loading and subsetting the data,
#'  fitting the Bayesian models, and generating diagnostic figures. Set up to loop over
#'   multiple PFTs and components. 
#'   Writes raw MCMC and PDF of diagnositcs to file and returns table of summary stats.
#'   
#' @details There are two usages of this function. 
#' When running "online" (connected to the PEcAn database), pass the database connection, con, and the pfts subsection of the PEcAn settings.
#' When running "stand alone" pass the pft list mapping species to species codes and the file paths to the allometry table and field data (optional)
#' 
#' @examples 
#' 
#' if(FALSE){
#'   pfts = list(FAGR = data.frame(spcd=531,acronym="FAGR"))
#'   allom.stats = AllomAve(pfts,ngibbs=500)
#' 
#'   ## example of a PFT with multiple species (late hardwood)
#'   ## note that if you're just using Jenkins the acronym column is optional
#'   pfts = list(LH = data.frame(spcd = c(531,318),acronym=c("FAGR","ACSA3")))
#' }
#' 
#' @author Michael Dietze
#' 
#' @export
#' 
AllomAve <- function(pfts,components=6,outdir=NULL,con=NULL,field=NULL,
                     parm=system.file("data/Table3_GTR-NE-319.v2.csv", package = "PEcAn.allometry"),
                     ngibbs=5000,nchain=3,
                     dmin=0.1,dmax=500){
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
  for(ipft in 1:length(pfts)){  ## loop over PFTs
    pft = pfts[[ipft]]
    pft.name = names(pfts)[ipft]
    allom.stats[[pft.name]] = list()
    
    for(component in components){ 
      print(c(pft,component))
      
      ## load data
      if(!is.null(con)){
        ### If running within PEcAn, grab the data from the database
        pft.name = pft$name
        allom <- query.allom.data(pft.name,component,con)
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
          allom.out = allom.BayesFit(allom,ngibbs,"exp",dmin,dmax)
        } else {
          allom.out = allom.BayesFit(allom,ngibbs,dmin=dmin,dmax=dmax)
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
      outfile = paste(outdir,paste("/Allom",pft.name,component,"Rdata",sep="."),sep="")
      print(c("saving MCMC output to",outfile))
      save(mc,DIC,DICg,pD,pDg,obs,allom,file=outfile)

      allom.stats[[pft.name]][[component]] = summary(mc)  
      allom.stats[[pft.name]][[component]]$cov = cov(as.matrix(mc))
      
      ## Save Posterior information (Pass to update.posterior module)
      if(FALSE){
        
      }
      
      ## Analysis/visualization
      pdffile = paste(outdir,paste("/Allom",pft.name,component,"MCMC","pdf",sep="."),sep="")
      print(c("saving diagnostic graphs to",pdffile))
      pdf(pdffile)
        
        ## specifying which rows were used in the fit & should be graphed
        ntally  <- which(nu(allom[['parm']][,"Xmax"])>=dmin & nu(allom[['parm']][,"Xmin"])<=dmax); 
            if(is.null(ntally)) ntally = 0;

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
        dseq = seq(dmin,dmax,length=100)  ## diameter sequence
        beta = allom.stats[[pft.name]][[component]]$statistics[,"Mean"]
        y.0  = exp(beta['mu0'] + beta['mu1']*log(dseq))
        y.g  = exp(beta['Bg0'] + beta['Bg1']*log(dseq))  
        y.o  = predict.allom.orig(dseq,allom$parm[ntally,])    
        lines(dseq,y.0,lwd=2,col=1)
        lines(dseq,y.g,lwd=2,col=2)
        for(i in seq_len(nrow(y.o))){
        	lines(dseq,y.o[i,],col=i+2)
        }
        legend("topleft",legend=c("Hier","global", paste("allom", ntally),lwd=c(2,2, rep(1, nrow(y.o))),col=1:(2+nrow(y.o)))
      
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

predict.allom.orig <- function(x,parm){

    out <- matrix(NA,nrow(parm),length(x))

	  eqn   = nu(parm$eqn)
	  a     = nu(parm$a)
	  b     = nu(parm$b)
	  c     = nu(parm$c)
	  d     = nu(parm$d)
	  e     = nu(parm$e)
	  Xcor  = nu(parm$Xcor)
	  Ycor  = nu(parm$Ycor)
	  Xtype = as.character(parm$Xtype)
	  
	for(i in 1:nrow(parm))	{

	  # X-unit conversion
      if(!is.na(Xcor[i])){
        x <- Xcor[i]*x
      }else{
        if(Xtype[i] == "d.b.h.^2"){
          ## convert to sq inches
          x = x*x/(2.54*2.54)
        } else {
          x = x*x*pi/4 ## convert to cm Basal Area
        }
      }

	
	  if(eqn[i] == 1){
        if(b[i] == 0 & c[i] > 0) b[i] = 1
        if(c[i] == 0 & b[i] > 0) c[i] = 1
        y = 10^(a[i] + b[i]*c[i]*log10(x))
      } else if(eqn[i] == 2){
        if(is.na(d[i]) | d[i] == 0) d[i] <- 1
        y = exp(a[i] + b[i]*x + c[i]*d[i]*log(x))
      } else if(eqn[i] == 3){
        y = exp(a[i] + b[i]*log(x) + c[i]*(d[i]+(e[i]*log(x))))
      } else if(eqn[i] == 4){
        if(is.na(d[i])) d[i] <- 0
        y = a[i] + b[i]*x + c[i]*x^d[i]
      } else if(eqn[i] == 5){
        y = a[i] + b[i]*x + c[i]*x^2 + d[i]*x^3
      } else if(eqn[i] == 6){
        y = a[i] *(exp( b[i] + (c[i]*log(x)) + d[i]*x))
      } else if(eqn[i] == 7){
        y = a[i] + ((b[i]*(x^c[i]))/((x^c[i])+ d[i]))
      } else if(eqn[i] == 8){
        y = 10^(a[i] + b[i]*log10(x))
      }else if(eqn[i] == 9){
        y = exp(log(a[i]) + b[i]*log(x))
      }else if(eqn[i] == 10){
        y = exp(a[i] + b[i]*log(x))
      }else if(eqn[i] == 11){
        if(is.na(b[i])) b[i] <- 0
        y = a[i]*x^(b[i])
      }
      out[i,] <- y*Ycor[i]
   }
   
	return(out)
}