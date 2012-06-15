#--------------------------------------------------------------------------------------------------#
##'
##' @title Start ED2 model runs
##' @name start.runs.ed
##' 
##'
start.runs.ed <- function(){
  ### TODO: Old code, needs to be updated.  NOT YET WORKING
  
  ## Priority only needs to be set for very big jobs as with ED2
  ## priority can be NULL in settings, if null,
  ## only batch.jobs.sh is required to exist
  if(is.null(settings$run$priority)){
    batch.jobs.script <- system.file("data", "batch.jobs.sh", package="PEcAn.ED")
  } else if (as.numeric(settings$run$priority) < 0) {
    reg.prior <- system.file("data", "batch.jobs.sh", package="PEcAn.ED")
    low.prior <- system.file("data", "batch.jobs.lowp.sh", package="PEcAn.ED")
    file.copy(reg.prior, low.prior, overwrite = TRUE)
    ## set priority 
    system(paste("sed 's/\"qsub/\"qsub\ -p\ ", settings$run$priority,
                 "/g' >", low.prior, sep = ''))
    batch.jobs.script <- low.prior
  } else if (as.numeric(settings$run$priority) > 0){
    stop("need admin rights to set higher priority")
  }
  
} ### ENd of function
