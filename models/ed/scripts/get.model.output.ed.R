#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
# library(XML)
# if(interactive()){
#   user <- Sys.getenv('USER')
#   options(error = browser)
#   if(user == 'dlebauer'){
#     settings.file = '~/in/ebifarm/prior/ebifarm.pavi.xml'
#   } else if(user == 'davids14') {
#     settings.file = '~/pecan/tundra.xml'
#   } else {
#     paste('please specify settings file in meta.analysis.R')
#   }
# } else {
#   settings.file <- Sys.getenv("PECANSETTINGS")
#   ## settings.file <- commandArgs(trailingOnly=TRUE)
# } 
# 
# settings.xml <- xmlParse(settings.file)
# settings <- xmlToList(settings.xml)

### TODO:  Update this code to work within new PEcAn organization.  Remove hard coded paths to source

get.model.output.ed <- function(){
  
  # This should no longer be needed. Depreciate.
  #if(!is.null(settings$Rlib)){ .libPaths(settings$Rlib)}
  if(settings$run$host$name == 'localhost'){
    send.files <- function(x){
      file.copy(from = x,
                to   = settings$outdir,
                overwrite = TRUE)
    }
    lapply( c(paste(settings$pecanDir,c('R/utils.R', 'R/model.specific.R', 
                                        'rscripts/read.output.R'),sep=""), 
              paste(settings$outdir, 'samples.Rdata', sep = '')), send.files)
    setwd(settings$outdir)
    source('read.output.R')
  } else { ## if not on localhost
    send.files <- function(filename){
      rsync(from = filename,
            to   = paste(settings$run$host$name, ':',settings$run$host$outdir, sep = ''))
    }
    lapply(c(paste(settings$outdir, 'samples.Rdata ', sep = ''),
             paste(settings$pecanDir, c('R/utils.R',
                                        'R/model.specific.R',
                                        'rscripts/read.output.R'),sep = '')),
           send.files)
    system(paste("ssh -T", settings$run$host$name, "'",
                 "cd", settings$run$host$outdir, "; R --vanilla < read.output.R'"))
    
    rsync(from = paste(settings$run$host$name, ':', settings$run$host$outdir, 'ensemble.Rdata', sep=''),
          to = settings$outdir)
    rsync(from = paste(settings$run$host$name, ':', settings$run$host$outdir, 'sensitivity.Rdata', sep=''),
          to = settings$outdir)
    }
}
## debugging
## source('rscripts/get.model.output.R', echo = TRUE, print.eval = TRUE, verbose = TRUE)
