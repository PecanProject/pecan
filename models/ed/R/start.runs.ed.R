                                        #-------------------------------------------------------------------------------
##Copyright (c) 2012 University of Illinois, NCSA.
##All rights reserved. This program and the accompanying materials
##are made available under the terms of the 
##University of Illinois/NCSA Open Source License
##which accompanies this distribution, and is available at
##http://opensource.ncsa.illinois.edu/license.html
                                        #-------------------------------------------------------------------------------
                                        #--------------------------------------------------------------------------------------------------#
##' 
##' Start ED2 model runs on local or remote server
##' @title Start ED2 model runs
##' @name start.runs.ED2
##' @export
##' @author David LeBauer, Shawn Serbin, Carl Davidson
start.runs.ED2 <- function(){
  
  host     <-  settings$run$host
  
  ## TODO fix issue #1173
  if (file.exists(paste(settings$outdir, 'launcher.sh', sep='/'))) {
    oldwd <- getwd()
    setwd(settings$outdir)
    system2(paste(settings$outdir, 'launcher.sh', sep='/'), wait=TRUE)
    setwd(oldwd)
  } else {
    ### need special script if priority < 0
    if(is.null(settings$run$priority)){
      batch.jobs.script <- system.file("batch.jobs.sh", package="PEcAn.ED")
      
    } else if (as.numeric(settings$run$priority) < 0) {
      batch.jobs.script.lowp <- system.file("batch.jobs.lowp.sh", package="PEcAn.ED")
      batch.jobs.script.lowp <- readLines(con=batch.jobs.script.lowp, n=-1)
      batch.jobs.script.lowp <- gsub('@PRIOR@', settings$run$priority, batch.jobs.script.lowp)
      writeLines(batch.jobs.script.lowp, con = paste(settings$outdir,"/batch.jobs.script.lowp.sh" , sep=''))
      batch.jobs.script <- file.path(paste(settings$outdir,"/batch.jobs.script.lowp.sh" , sep=''))   
      
    } else if (as.numeric(settings$run$priority) > 0){
      stop("need admin rights to set higher priority")
    }
    
    ## if using ED, write runscript that rsyncs at the end
    if(any(grep("ED", settings$run$host$rundir))){ #if using ED
      write.run.ED(settings)
    }
    
                                        #Run model from user made bash script 
    if(host$name == 'localhost') {
      system(paste('cd ', host$rundir, ';',
                   settings$pecanDir, batch.jobs.script, sep = ''))
    }else{

      system(paste("echo 'cd ", host$rundir, "' | ",                
                   "cat - ", batch.jobs.script,
                   " | ",'ssh -T ', host$name, sep = ''))                                                       
##      rsync(args = " -routi ", from = batch.jobs.script,
##            to = paste(host$name, ":",host$rundir, sep = ""))
##      system(paste("ssh ", host$name, " '",
##                   paste("cd ", host$rundir, "; ", "./", basename(batch.jobs.script), sep = "") ,"'", sep = ""))
    }
    
  }
  
} ### End of function
                                        #==================================================================================================#


####################################################################################################
### EOF.  End of R script file.          		
####################################################################################################
