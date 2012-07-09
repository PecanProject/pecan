#--------------------------------------------------------------------------------------------------#
##' 
##' Start ED2 model runs on local or remote server
##' @title Start ED2 model runs
##' @name start.runs.ed
##' @export
##' @author David LeBauer, Shawn Serbin, Carl Davidson
start.runs.ED2 <- function(){
  ### TODO: Old code, needs to be updated.  Could can make this a generalized script in
  ### utils package
  
  host     <-  settings$run$host
  
  ## Priority only needs to be set for very big jobs as with ED2
  ## priority can be NULL in settings, if null,
  ## only batch.jobs.sh is required to exist
  if(is.null(settings$run$priority)){
    batch.jobs.script <- system.file("batch.jobs.sh", package="PEcAn.ED")
    
  } else if (as.numeric(settings$run$priority) < 0) {
    batch.jobs.script <- system.file("batch.jobs.lowp.sh", package="PEcAn.ED")

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
                 "cat - ", batch.jobs.script, " | ",
                 'ssh -T ', host$name, sep = ''))
  }
  
} ### End of function
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.          		
####################################################################################################
