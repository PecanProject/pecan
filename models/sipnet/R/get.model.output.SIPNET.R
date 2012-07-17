#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------#

##' Reads the output of a single model run
##'
##' @title Read output
##' @name read.output.SIPNET
##' @param run.id the id distiguishing the model run
##' @param outdir the directory that the model's output was sent to
##' @param start.year 
##' @param end.year
##' @return vector of output variable
##' @export
##' @author Michael Dietze
read.output.SIPNET <- function(run.id, outdir, start.year=NA, end.year=NA,variables="GPP"){

  ## call to generic read output. function read.output.SIPNET to be eliminated once ED2 is generalized
  read.output(run.id,outdir,start.year,end.year,variables,model="SIPNET")
  
}
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##' Function to retrieve model output from local or remote server
##'
##' @name get.model.output.SIPNET
##' @title Retrieve model output from local or remote server
##' 
##' @import PEcAn.utils
##' @export
##'

### *** THIS WHOLE FUNCTION SHOULD BE MADE INTO A GENERIC CASE IN UTILS THAT JUST HAS A FEW MODEL SPECIFIC PIECES OF INFO PASSED TO IT *** 

get.model.output.SIPNET <- function(){
  
  ### Get model output on the localhost
  if(settings$run$host$name == 'localhost'){

    olddir <- getwd()
    setwd(settings$outdir)
    get.results("SIPNET")
    setwd(olddir)
    
  } else {

    ## model output is on a remote host
        
    ### Make a copy of required functions and place in file PEcAn.functions.R
    dump(c("get.run.id","read.ensemble.output","read.sa.output","read.output.generic","get.results"),
         file=paste(settings$outdir,"PEcAn.functions.R",sep=""))
    
    ### Add execution of get.results to the end of the PEcAn.functions.R file
    ### This will execute all the code needed to extract output on remote host
    cat("get.results()",file=paste(settings$outdir,"PEcAn.functions.R",sep=""),
        append=TRUE)

    ### Copy required PEcAn.functions.R to remote host
    rsync('-outi',paste(settings$outdir,"PEcAn.functions.R",sep=""),
          paste(settings$run$host$name, ':',settings$run$host$outdir, sep = '') )

    ### Run script on remote host
    system(paste("ssh -T", settings$run$host$name, "'",
             "cd", settings$run$host$outdir, "; R --vanilla < PEcAn.functions.R'"))
    
    ### Get PEcAn output from remote host
    rsync('-outi', from = paste(settings$run$host$name, ':', settings$run$host$outdir, 'output.Rdata', sep=''),
      to = settings$outdir)

  } ### End of if/else
  
} ### End of function
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.              
####################################################################################################
