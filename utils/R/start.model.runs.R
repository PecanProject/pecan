#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------#
##'
##' Start selected ecosystem model runs within PEcAn workflow
##' 
##' @name start.model.runs
##' @title Start ecosystem model runs
##' @export 
##' @examples
##' \dontrun {
##' start.model.runs("ED2")
##' start.model.runs("SIPNET")
##' }
##' @author Shawn Serbin
##'
start.model.runs <- function(model){

  fcn.name <- paste("start.runs.",model,sep="")
  if(exists(fcn.name)){
    print(" ")
    print("-------------------------------------------------------------------")
    print(paste(" Starting model runs",model))
    print("-------------------------------------------------------------------")
    print(" ")
    
	# write to the runs table
	con <- try(query.base.con(settings),silent=TRUE)
  if(!is.character(con)){
  	query.base(paste("INSERT INTO runs (model_id, site_id, start_time, finish_time, outdir, created_at, started_at) values ('", settings$model$id, "', '", settings$run$site$id, "', '", settings$run$start.date, "', '", settings$run$end.date, "', '",settings$outdir , "', NOW(), NOW())", sep=''), con)
	  id <- query.base(paste("SELECT LAST_INSERT_ID() AS ID"), con)
  }
    
	# launch actual model
	do.call(fcn.name,args=list())
	
	# job is finished
	# TODO this should move in case of launch of on HPC
  if(!is.character(con)){
  	query.base(paste("UPDATE runs SET finished_at =  NOW() WHERE id = ", id), con)
	  query.close(con)
  }
	
  } else {
    warning(paste(fcn.name,"does not exist"))
    warning(paste("This function is required, please make sure the model module is loaded for",model))
    stop()
  }

  
} ### End of function
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.              
####################################################################################################
