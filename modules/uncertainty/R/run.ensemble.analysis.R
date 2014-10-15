#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#--------------------------------------------------------------------------------------------------#
##' run ensemble.analysis
##' 
##' @name run.ensemble.analysis
##' @title run ensemble.analysis
##' @return nothing, creates ensemble plots as ensemble.analysis.pdf
##' @param plot.timeseries if TRUE plots a modeled timeseries of target variable(s) with CIs
##' @export
##' @author David LeBauer, Shawn Serbin
##'
run.ensemble.analysis <- function(plot.timeseries=NA){
 
  if(!exists("settings")){ # temporary hack
                        # waiting on http://stackoverflow.com/q/11005478/199217
    settings <- list(outdir = "/tmp/",
                     pfts = list(pft = list(name = "ebifarm.pavi", outdir = "/tmp/")),
                     ensemble.analysis = NULL)
  }

  cflux <- c("GPP","NPP","NEE","TotalResp","AutoResp","HeteroResp","DOC_flux","Fire_flux") #converted to gC/m2/s
  wflux <- c("Evap","TVeg","Qs","Qsb","Rainf") #kgH20 m-2 s-1

  variables <- settings$ensemble$variable #grab target variable(s) from pecan.xml
  print(paste("----- Variable: ",variables,sep=""))

  if (length(variables) != 1) {
    logger.error("Only computing first variable for now.")
  }
  #units <- lapply(variables, function(x) { paste0(x, " (", mstmipvar(x, silent=TRUE)$units, ")") })
  units <- paste0(variables[1], " (", mstmipvar(variables[1], silent=TRUE)$units, ")")

  ### Check if ensemble was run and was larger than 0
  if ('ensemble' %in% names(settings) & settings$ensemble$size>0) {
    ### Load parsed model results
    load(file.path(settings$outdir, 'ensemble.Rdata'))
  }
  
  ### ------------------- Start ensemble analysis -------------------
  ensemble.results <- list()
  if (is.null(settings$run$site$name)){
    print("----- Running ensemble analysis -----")
  } else{
    print(paste("----- Running ensemble analysis for site: ",settings$run$site$name))
  }
  
  ## Generate ensemble figure
  #fig.out <- settings$pfts$pft$outdir
  fig.out <- settings$outdir # main output directory
  start.year <- ifelse(is.null(settings$ensemble$start.year), NA, settings$ensemble$start.year)
  end.year   <- ifelse(is.null(settings$ensemble$end.year), NA, settings$ensemble$end.year)
  ftime = ifelse(is.na(start.year),"",
                 ifelse(end.year==start.year,paste0(".",start.year),
                        paste0(".",start.year,"-",end.year)))
  fname = paste0("ensemble.analysis.",variables[1],ftime,".pdf")
  
  pdf(file=file.path(fig.out,fname),width=13,height=6)
  par(mfrow=c(1,2),mar=c(4,4.8,1,2.0)) # B, L, T, R
  hist(unlist(ensemble.output),xlab=units,
       main="",cex.axis=1.1,cex.lab=1.4,col="grey85")
  box(lwd=2.2)
  
  boxplot(unlist(ensemble.output),ylab=units,
          boxwex=0.6,col="grey85", cex.axis=1.1,range=2,pch=21,cex=1.4, bg="black",cex.lab=1.5)
  box(lwd=2.2)

  dev.off()
  
  print("----- Done!")
  print(" ")
  print("-----------------------------------------------")
  print(" ")
  print(" ")
  
  ### Plot ensemble time-series
  if (!is.na(plot.timeseries)){
    pdf(file.path(settings$outdir,"ensemble.ts.pdf"),width=12,height=9)    
    ensemble.ts(read.ensemble.ts(settings$model$type))
    dev.off()
  }

} ### End of function
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##'
##' Reads ensemble time-series from PEcAn for the selected target variables
##'
##' @name read.ensemble.ts
##' @title Reads an ensemble time-series from PEcAn for the selected target variables
##' @return list
##'
##' @export
##'
##' @author Michael Dietze 
##'
read.ensemble.ts <- function(model){

  ## SETTINGS  
  ensemble.ts <- list()
  ensemble.size <- as.numeric(settings$ensemble$size)
  outdir <- settings$modeloutdir
  #outdir <- settings$run$host$outdir
  start.year <- ifelse(is.null(settings$ensemble$start.year), NA, settings$ensemble$start.year)
  end.year   <- ifelse(is.null(settings$ensemble$end.year), NA, settings$ensemble$end.year)

  variables <- NULL
  if("ensemble" %in% names(settings)){
    if("variable" %in% names(settings$ensemble)){
      var <- which(names(settings$ensemble) == 'variable')

      for(i in 1:length(var)){
        variables[i] = settings$ensemble[[var[i]]]
      }
    }
  }
  if (is.null(variables)) {
    variables <- "NPP"
  }
  print(paste("----- Variable: ",variables,sep=""))
  print("----- Reading ensemble output ------")

  if (exists('runs.samples')) {
    ensemble.runs <- runs.samples$ensemble
  } else {
    ensemble.runs <- list()
    samples.file <- file.path(settings$outdir, 'samples.Rdata')
    print(samples.file)
    if(file.exists(samples.file)){
      load(samples.file)
      ensemble.runs <- runs.samples$ensemble
    } else {
      stop(samples.file, "not found required by read.ensemble.output")      
    }
  }

  ## read ensemble output
  for(row in rownames(ensemble.runs)) {
    
    #print(paste("The value for rownames(ensemble.runs) is:  ",rownames(ensemble.runs),sep=""))
    run.id <- ensemble.runs[row, 'id']
    print(run.id)
    newrun <- read.output(run.id, file.path(outdir, run.id), start.year, end.year, variables)

    for(j in 1:length(variables)){
      if(as.numeric(row) == 1){
        ensemble.ts[[j]] <- matrix(NA,ensemble.size,length(newrun[[j]]))
      }
      ensemble.ts[[j]][as.numeric(row),] <- newrun[[j]]
    }    
  }

  names(ensemble.ts) <- variables
  # BMR 10/16/13 Save this variable now to operate later on
  save(ensemble.ts, file = file.path(settings$outdir,"ensemble.ts.Rdata"))
  return(ensemble.ts)

}


filterNA <- function(x,w){
  y <- rep(NA,length(x))
  for(i in 1:length(x)){
    y[i] = mean(x[i:(min(length(x),i+w))],na.rm=TRUE)    
  }
  return(y)
}

#--------------------------------------------------------------------------------------------------#
##'
##' Plots an ensemble time-series from PEcAn for the selected target variables
##'
##' @name ensemble.ts
##' @title Plots an ensemble time-series from PEcAn for the selected target variables
##' @return nothing, generates an ensemble time-series plot
##'
##' @export
##'
##' @author Michael Dietze 
##'
ensemble.ts <- function(ensemble.ts,observations=NULL,window=1){

  print("------ Generating ensemble time-series plot ------")
  variables = names(ensemble.ts)


  ## temporary check for plots that should be >0
  nonzero = c("GPP","TotalResp","AutoResp","HeteroResp","Evap","TVeg")
  
  ## should probably add an extraction of the time axis from the first ensemble member

  ## should probably add extraction of meta-data from netCDF files
  
  ## plot
  for(j in 1:length(variables)){
    
    if(window > 1){
#      myens <- apply(ensemble.ts[[j]],1,filterNA,window)#rep(1/window,window))
      myens <- t(apply(ensemble.ts[[j]],1,function(x){
        tapply(x,rep(1:(length(x)/window+1),each=window)[1:length(x)],mean,na.rm=TRUE)
      }))
      
    } else {
      myens <- ensemble.ts[[j]]
    }    

    ens.mean = apply(myens,2,mean,na.rm=TRUE)
    CI = apply(myens,2,quantile,c(0.025,0.5,0.975),na.rm=TRUE)
    ylim = range(CI,na.rm=TRUE)
    
    ### temporary fix to values less than zero that are biologically unreasonable (e.g. GPP)
    if (variables[j] %in% nonzero){
      ylim <- c(0,ylim[2])
    }
    
    plot(ens.mean,ylim=ylim,lwd=2,xlab="time",ylab=variables[j],main=variables[j],
         type="l")

    ### Code to be updated with polygon (below)
    #for(i in 1:nrow(CI)){
    #  lines(CI[i,],col=2,lty=c(2,1,2),lwd=c(1.2,1.0,1.2))
    #}
    lines(CI[1,],col=2,lty=2,lwd=1.2)
    #lines(CI[2,],col="dark grey",lty=1,lwd=1.5)
    lines(CI[3,],col=2,lty=2,lwd=1.2)

    ## generate plot polygon using CIs
    #dims <- dim(CI)
    #poly <- 1:dims[2]
    #polygon(c(poly ,rev(poly)),c(CI[3,], rev(CI[1,])),col="#99CC99",border=NA)
    ##

    ## plot mean over others again
#    lines(ens.mean,col="black",lwd=1.5)
#    lines(CI[2,],col="dark grey",lty=1,lwd=1.5)
    
    if(!is.null(observations)){
      if(window==1){
        fobs <- observations
      } else {
        fobs <- tapply(observations,rep(1:(length(observations)/window+1),each=window)[1:length(observations)],mean,na.rm=TRUE)
      }
      #lines(filter(observations,rep(1/window,window)),col=2,lwd=1.5)
      #lines(filterNA(observations,window),col=2,lwd=1.5)
      points(fobs,col=3,lwd=1.5)
    }

    ## show legend
    legend("topleft",legend=c("mean","95% CI","data"),lwd=3,col=c(1,2,3),lty=c(1,2,1))
    ## add surrounding box to plot
    box(lwd=2.2)
  }
  ensemble.analysis.results <- list()
  ensemble.analysis.results$mean <- ens.mean
  ensemble.analysis.results$CI <- CI
  
  save(ensemble.analysis.results,
       file = file.path(settings$outdir,
                    "ensemble.ts.analysis.results.Rdata"))
  
}
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.        			
####################################################################################################
