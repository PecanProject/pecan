#--------------------------------------------------------------------------------------------------#
##' Function to retrieve ED2 HDF model output from local or remote server
##'
##' @name get.model.output.ed
##' @title Retrieve ED2 HDF model output from local or remote server
##' 
##' @import PEcAn.utils
##' @export
##'
get.model.output.ed <- function(){
  
  ### Get ED2 model output on the localhost
  if(settings$run$host$name == 'localhost'){
    
    ### Move required functions to host
    ## TODO: take out functions read.output.file.ed & read.output.ed from write.configs.ed &
    ## put into a new file specific for reading ED output
    dump(c("get.run.id","abbreviate.run.id.ED","left.pad.zeros","read.ensemble.output",
           "read.sa.output","read.output.file.ed"),
         file=paste(settings$outdir,"PEcAn.scripts.R",sep=""))
    
    ### Is the previous necessary for localhost?  These functions should be availible within R
    ### & should not need to be copied and run but could instead be called within the running R
    ### shell.  SPS
    
    setwd(settings$outdir)
    source('PEcAn.scripts.R')
    ### If running on remote host
  } else {
    
    ### Make a copy of required functions and place in file PEcAn.scripts.R
    dump(c("get.run.id","abbreviate.run.id.ED","left.pad.zeros","read.ensemble.output",
           "read.sa.output","read.output.file.ed"),
         file=paste(settings$outdir,"PEcAn.scripts.R",sep=""))

    ### Copy PEcAn.scripts.R to remote host
    rsync('-outi',paste(settings$outdir,"PEcAn.scripts.R",sep=""),
          paste(settings$run$host$name, ':',settings$run$host$outdir, sep = '') )

    ### Run script on remote host
    system(paste("ssh -T", settings$run$host$name, "'",
             "cd", settings$run$host$outdir, "; R --vanilla < PEcAn.scripts.R'"))
    
    ### Get PEcAn output from remote host
    rsync('-outi', from = paste(settings$run$host$name, ':', settings$run$host$outdir, 'output.Rdata', sep=''),
      to = settings$outdir)

  } ### End of if/else
  
} ### End of function
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.              
####################################################################################################
