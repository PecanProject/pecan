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

  cflux = c("GPP","NPP","NEE","TotalResp","AutoResp","HeteroResp","DOC_flux","Fire_flux") #converted to gC/m2/s
  wflux = c("Evap","TVeg","Qs","Qsb","Rainf") #kgH20 m-2 s-1

  variables = settings$sensitivity.analysis$variable #grab target variable(s) from pecan.xml
  print(paste("---- Variable: ",variables,sep=""))

  ### Temp hack
  if (variables %in% cflux){
    units <- paste(variables[1],"(kgC/ha/year)")
  } else{
    units <- paste(variables[1],"(kgH2O/ha/year)")
  }
  
  ### Check if ensemble was run and was larger than 0
  if ('ensemble' %in% names(settings) & settings$ensemble$size>0) {
    ### Load parsed model results
    load(paste(settings$outdir, 'output.Rdata', sep=''))
  }
  
  ### ------------------- Start ensemble analysis -------------------
  ensemble.results <- list()
  if (is.null(settings$run$site$name)){
    print("----- Running ensemble analysis -----")
  } else{
    print(paste("----- Running ensemble analysis for site: ",settings$run$site$name))
  }
  
  ## Generate ensemble figure
  fig.out <- settings$pfts$pft$outdir
  
  pdf(file=paste(fig.out,"ensemble.analysis.pdf",sep=""),width=13,height=6)
  par(mfrow=c(1,2),mar=c(4,4.8,1,2.0)) # B, L, T, R
  hist(unlist(ensemble.output),xlab=units,
       main="",cex.axis=1.1,cex.lab=1.4,col="grey85")
  box(lwd=2.2)
  
  boxplot(unlist(ensemble.output),ylab=units,
          boxwex=0.6,col="grey85", cex.axis=1.1,range=2,pch=21,cex=1.4, bg="black",cex.lab=1.5)
  box(lwd=2.2)

  dev.off()
  
} ### End of function
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##'
##' Plots an ensemble time-series from PEcAn for the selected target variables
##'
##' @name ensemble.ts
##' @title Plots an ensemble time-series from PEcAn for the selected target variables
##' @return nothing, generates an ensemble time-series plot
##'
##' @author Michael Dietze 
##'
ensemble.ts <- function(){

  ## SETTINGS
  
  ensemble.ts <- list()
  ensemble.size <- as.numeric(settings$ensemble$size)
  outdir <- settings$outdir
    start.year <- ifelse(is.null(settings$sensitivity.analysis$start.year),
                       NA, settings$sensitivity.analysis$start.year)
  end.year   <- ifelse(is.null(settings$sensitivity.analysis$end.year),
                       NA, settings$sensitivity.analysis$end.year)

  variables = "NPP"
  if("sensitivity.analysis" %in% names(settings)){
    if("variable" %in% names(settings$sensitivity.analysis)){
      var = which(names(settings$sensitivity.analysis) == 'variable')
      for(i in 1:length(var)){
        variables[i] = settings$sensitivity.analysis[[var[i]]]
      }
    }
  }
  print(variables)

  ## read ensemble output
  for(i in 1:ensemble.size){
    run.id <- get.run.id('ENS', left.pad.zeros(i, 5))#log10(ensemble.size)+1))
    print(run.id)
    newrun <- read.output(run.id,outdir,start.year,end.year,variables,model)

    if(i == 1){
      for(j in 1:length(variables)){
        ensemble.ts[[j]] <- matrix(NA,ensemble.size,length(newrun[[j]]))
      }
    }

    ensemble.ts[[j]][i,] <- newrun[[j]]
    
  }

  ## should probably add an extraction of the time axis from the first ensemble member

  ## should probably add extraction of meta-data from netCDF files
  
  ## plot
  pdf(paste(outdir,"ensemble.ts.pdf",sep="/"))
  for(j in 1:length(variables)){
    ylim = range(ensemble.ts[[j]])
    plot(apply(ensemble.ts[[j]],2,mean),ylim=ylim,lwd=2,xlab="time",ylab=variables[j],main=variables[j])
    CI = apply(ensemble.ts[[j]],2,quantile,c(0.025,0.5,0.975))
    for(i in 1:nrow(CI)){
      lines(CI[i,],col=2,lty=c(2,1,2),lwd=2)
    }
    legend("topleft",legend=c("mean","median","95% CI"),lwd=3,col=c(1,2,2),lty=c(1,1,2))
  }
  dev.off()

}
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.        			
####################################################################################################