#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------------#
##' Function to retrieve model output from local or remote server
##'
##' @name get.model.output.DALEC
##' @title Retrieve model output from local or remote server
##' 
##' @import PEcAn.utils
##' @export
##'
get.model.output.DALEC <- function(settings){
  
  ### Get model output on the localhost
  if(settings$run$host$name != 'localhost'){
        
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
    rsync('-outi', from = paste(settings$run$host$name, ':', settings$run$host$outdir, 'ensemble.Rdata', sep=''),
      to = settings$outdir)
    rsync('-outi', from = paste(settings$run$host$name, ':', settings$run$host$outdir, 'sensitivity.Rdata', sep=''),
      to = settings$outdir)

  } ### End of if/else
  
} ### End of function
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.              
####################################################################################################
