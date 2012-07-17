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
  
  variables = "GPP" # hack for now.  need to be able to dynamically update.
  
  if(!exists("settings")){ # temporary hack
                        # waiting on http://stackoverflow.com/q/11005478/199217
    settings <- list(outdir = "/tmp/",
                     pfts = list(pft = list(name = "ebifarm.pavi",
                                   outdir = "/tmp/")),
                     ensemble.analysis = NULL)
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
  hist(unlist(ensemble.output),xlab=expression(paste("GPP (",mu*mols~m^{-2}~s^{-1},")")),
       main="",cex.axis=1.1,cex.lab=1.4,col="grey85")
  box(lwd=2.2)
  
  boxplot(unlist(ensemble.output),ylab=expression(paste("GPP (",mu*mols~m^{-2}~s^{-1},")")),
          boxwex=0.6,col="grey85", cex.axis=1.1,range=2,pch=21,cex=1.4, bg="black",cex.lab=1.5)
  box(lwd=2.2)

  dev.off()
  
} ### End of function
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.        			
####################################################################################################
