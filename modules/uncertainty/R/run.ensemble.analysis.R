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
##' @author David LeBauer, Shawn Serbin, Ryan Kelly
##'
run.ensemble.analysis <- function(plot.timeseries=NA, ensemble.id=NULL,
                           variable=NULL, start.year=NULL, end.year=NULL, ...) {
                        
  if(FALSE) {
    plot.timeseries=NA
    ensemble.id=variable=start.year=end.year=NULL
  }

  if(!exists("settings")){ # temporary hack
                        # waiting on http://stackoverflow.com/q/11005478/199217
    settings <- list(outdir = "/tmp/",
                     pfts = list(pft = list(name = "ebifarm.pavi", outdir = "/tmp/")),
                     ensemble.analysis = NULL)
  }
  
  
  # Set variable and years. Use args first, then settings, then defaults/error
  if(is.null(ensemble.id)) ensemble.id <- settings$ensemble$ensemble.id
  if(is.null(ensemble.id)) {
    # Try to just grab the most recent one
    ens.ids <- dir(file.path(settings$outdir, "ensemble"))
    if(length(ens.ids) > 0) {
      ensemble.id <- max(ens.ids)
    } else {
      if(is.null(ensemble.id)) logger.severe("Can't find a valid ensemble for ensemble analysis!")
    }
  }
  
  if(is.null(start.year)) start.year <- settings$ensemble$start.year
  if(is.null(end.year)) end.year <- settings$ensemble$end.year
  if(is.null(start.year) | is.null(end.year)) logger.severe("No years given for ensemble analysis!")

  if(is.null(variable)) {
    if("variable" %in% names(settings$ensemble)){
      var <- which(names(settings$ensemble) == 'variable')
      for(i in 1:length(var)){
        variable[i] = settings$ensemble[[var[i]]]
      }
    }
  }
  if(is.null(variable)) logger.severe("No variables for ensemble analysis!")
  
  # Only handling one variable at a time for now
  if(length(variable) > 1) {
    variable <- variable[1]
    logger.warn(paste0("Currently performs ensemble analysis on only one variable at a time. Using first (", variable, ")"))
  }

  cflux <- c("GPP","NPP","NEE","TotalResp","AutoResp","HeteroResp","DOC_flux","Fire_flux") #converted to gC/m2/s
  wflux <- c("Evap","TVeg","Qs","Qsb","Rainf") #kgH20 m-2 s-1

  print(paste("----- Variable: ",variable,sep=""))

  #units <- lapply(variable, function(x) { paste0(x, " (", mstmipvar(x, silent=TRUE)$units, ")") })
  units <- paste0(variable[1], " (", mstmipvar(variable[1], silent=TRUE)$units, ")")

  ### Load parsed model results
  fname <- ensemble.filename(settings, "ensemble.output", "Rdata", all.var.yr=FALSE,
    ensemble.id=ensemble.id, variable=variable, start.year=start.year, end.year=end.year)
  load(fname)

  
  ### ------------------- Start ensemble analysis -------------------
  ensemble.results <- list()
  if (is.null(settings$run$site$name)){
    print("----- Running ensemble analysis -----")
  } else{
    print(paste("----- Running ensemble analysis for site: ",settings$run$site$name))
  }
  
  ## Generate ensemble figure
  fname <- ensemble.filename(settings, "ensemble.analysis", "pdf", all.var.yr=FALSE,
    ensemble.id=ensemble.id, variable=variable, start.year=start.year, end.year=end.year)
  
  pdf(file=fname,width=13,height=6)
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
    fname <- ensemble.filename(settings, "ensemble.ts", "pdf", all.var.yr=FALSE,
      ensemble.id=ensemble.id, variable=variable, start.year=start.year, end.year=end.year)

    pdf(fname,width=12,height=9)    
      ensemble.ts.analysis <- ensemble.ts(read.ensemble.ts(settings$model$type), ...)
    dev.off()
    
    fname <- ensemble.filename(settings, "ensemble.ts.analysis", "Rdata", all.var.yr=FALSE,
      ensemble.id=ensemble.id, variable=variable, start.year=start.year, end.year=end.year)
    save(ensemble.ts.analysis, file=fname)
  }

} ### End of function
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##'
##' Reads ensemble time-series from PEcAn for the selected target variable
##'
##' @name read.ensemble.ts
##' @title Reads an ensemble time-series from PEcAn for the selected target variable
##' @return list
##'
##' @export
##'
##' @author Michael Dietze, Ryan Kelly
##'
read.ensemble.ts <- function(model, ensemble.id=NULL, variable=NULL, start.year=NULL, end.year=NULL) {

  # Set variable and years. Use args first, then settings, then defaults/error
  if(is.null(start.year)) start.year <- settings$ensemble$start.year
  if(is.null(end.year)) end.year <- settings$ensemble$end.year
  if(is.null(start.year) | is.null(end.year)) logger.severe("No years given for ensemble analysis!")

  if(is.null(variable)) {
    if("variable" %in% names(settings$ensemble)){
      var <- which(names(settings$ensemble) == 'variable')
      for(i in 1:length(var)){
        variable[i] = settings$ensemble[[var[i]]]
      }
    }
  }
  if(is.null(variable)) logger.severe("No variables for ensemble analysis!")
  
  # Only handling one variable at a time for now
  if(length(variable) > 1) {
    variable <- variable[1]
    logger.warn(paste0("Currently performs ensemble analysis on only one variable at a time. Using first (", variable, ")"))
  }

  print(paste("----- Variable: ",variable,sep=""))
  print("----- Reading ensemble output ------")

  ### Load ensemble run IDs
  # Can specify ensemble ids manually. If not, look in settings. If none there, just look in samples.Rdata, which for backwards compatibility still contains the sample info for (the most recent)  sensitivity and ensemble analysis combined.
  if(!is.null(ensemble.id)) {
    fname <- ensemble.filename(settings, "ensemble.samples", "Rdata", 
               ensemble.id=ens.ensemble.id, all.var.yr=TRUE)
  } else if(!is.null(settings$ensemble$ensemble.id)) {
    ensemble.id <- settings$ensemble$ensemble.id
    fname <- ensemble.filename(settings, "ensemble.samples", "Rdata", 
               ensemble.id=ensemble.id, all.var.yr=TRUE)
  } else {
    fname <- file.path(settings$outdir, 'samples.Rdata')
  }
  if(!file.exists(fname)) logger.severe("No ensemble samples file found!")
  load(fname)
  
  # For backwards compatibility, define ens.run.ids if not just loaded
  if(!exists("ens.run.ids"))   ens.run.ids <- runs.samples$ens

  ensemble.size <- nrow(ens.run.ids)

  ## read ensemble output
  # Leaving list output even though only one variable allowed for now. Will improve backwards compatibility and maybe help in the future.
  ensemble.ts <- list() 
  for(row in rownames(ens.run.ids)) {
    run.id <- ens.run.ids[row, 'id']
    print(run.id)
    newrun <- read.output(run.id, file.path(settings$outdir, "out", run.id), 
                           start.year, end.year, variable)

    for(j in 1:length(variable)){
      if(as.numeric(row) == 1){
        ensemble.ts[[j]] <- matrix(NA,ensemble.size,length(newrun[[j]]))
      }
      ensemble.ts[[j]][as.numeric(row),] <- newrun[[j]]
    }    
  }

  names(ensemble.ts) <- variable
  # BMR 10/16/13 Save this variable now to operate later on
  fname <- ensemble.filename(settings, "ensemble.ts", "Rdata", all.var.yr=FALSE,
    ensemble.id=ensemble.id, variable=variable, start.year=start.year, end.year=end.year)
  
  save(ensemble.ts, file=fname)
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
##' Plots an ensemble time-series from PEcAn for the selected target variable
##'
##' @name ensemble.ts
##' @title Plots an ensemble time-series from PEcAn for the selected target variable
##' @return nothing, generates an ensemble time-series plot
##'
##' @export
##'
##' @author Michael Dietze, Ryan Kelly
##'
ensemble.ts <- function(ensemble.ts,observations=NULL,window=1){
  print("------ Generating ensemble time-series plot ------")
  variable = names(ensemble.ts)

  ## temporary check for plots that should be >0
  nonzero = c("GPP","TotalResp","AutoResp","HeteroResp","Evap","TVeg")
  
  ## should probably add an extraction of the time axis from the first ensemble member

  ## should probably add extraction of meta-data from netCDF files
  
  ## plot
  for(j in 1:length(variable)){
    
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
    if (variable[j] %in% nonzero){
      ylim <- c(0,ylim[2])
    }
    
    plot(ens.mean,ylim=ylim,lwd=2,xlab="time",ylab=variable[j],main=variable[j],
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

  invisible(ensemble.analysis.results)  
}
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.        			
####################################################################################################
