#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------#
# Template for functions to prepare and write out files model-specific configuration files for MA
#--------------------------------------------------------------------------------------------------#
PREFIX_XML <- '<?xml version="1.0"?>\n<!DOCTYPE config SYSTEM "ed.dtd">\n'

convert.samples.DALEC <- function(trait.samples){
    
  DEFAULT.LEAF.C <- 0.48
  ## convert SLA from m2 / kg leaf to m2 / kg C 
  
  if('SLA' %in% names(trait.samples)){
    trait.samples[['SLA']] <- trait.samples[['SLA']] / DEFAULT.LEAF.C / 1000
  }
  
  if('leaf_turnover_rate' %in% names(trait.samples)){
    trait.samples[['leaf_turnover_rate']] <- trait.samples[['leaf_turnover_rate']]/365
    names(trait.samples)[which(names(trait.samples)=="leaf_turnover_rate")] <- "t5"
  }
  
  if('root_turnover_rate' %in% names(trait.samples)){
    trait.samples[['root_turnover_rate']] <- trait.samples[['root_turnover_rate']]/365
    names(trait.samples)[which(names(trait.samples)=="root_turnover_rate")] <- "t7"
  }
  
  return(trait.samples)
}

#--------------------------------------------------------------------------------------------------#
##' Writes a configuration files for your model
#--------------------------------------------------------------------------------------------------#
##' write Dalec Configuration files
##'
##' @title write.config.DALEC 
##' @param defaults 
##' @param trait.values 
##' @param settings 
##' @param run.id 
##' @return configuration files
##' @export write.config.DALEC
write.config.DALEC <- function(defaults, trait.values, settings, run.id){
  
  ### CONVERT PARAMETERS
  cmdFlags = ""
  for(group in names(trait.values)){
    if(group == "env"){
      
      ## set defaults from config.header
      
      ##
      
    } else {
      if(!is.null(trait.values[[group]])){
        params <- convert.samples.DALEC(trait.values[[group]])
        logger.info(names(params))
        for(i in 1:length(params)){
          cmdFlags <- paste(cmdFlags," -",names(params)[i]," ",params[[i]],sep="")
        }
      }    
    }
  }
  
  # find out where to write run/ouput
  rundir <- file.path(settings$run$host$rundir, as.character(run.id))
  outdir <- file.path(settings$run$host$outdir, as.character(run.id))
  if (is.null(settings$run$host$qsub) && (settings$run$host$name == "localhost")) {
    rundir <- file.path(settings$rundir, as.character(run.id))
    outdir <- file.path(settings$modeloutdir, as.character(run.id))
  }
  
  
  ### WRITE PARAMETERS
  config.file.name <- paste('CONFIG.',run.id, sep='')
  writeLines(cmdFlags, con = paste(rundir,"/", config.file.name, sep=''))
        
  ### WRITE JOB.SH
  jobsh = paste0("#!/bin/bash\n",settings$model$binary,
                 " $(cat ",rundir,"/",config.file.name,
                 ") < ",as.character(settings$run$inputs$met$path),
                 " > ",outdir,"/out.txt\n",
#                 'echo ".libPaths(',"'~/R/library');",
                 'echo "',
                 ' require(PEcAn.DALEC); model2netcdf.DALEC(',
                 "'",outdir,"',",
                 settings$run$site$lat,",",
                 settings$run$site$lon,", '",
                 settings$run$start.date,"', '",
                 settings$run$end.date,"') ",
                 '" | R --vanilla'
                 )
  writeLines(jobsh, con=file.path(settings$rundir, run.id, "job.sh"))
  Sys.chmod(file.path(settings$rundir, run.id, "job.sh"))
  
  
    ### Display info to the console.
    print(run.id)
}
#==================================================================================================#

remove.config.DALEC <- function(outdir,settings){
  
}


#--------------------------------------------------------------------------------------------------#
##'
##' @name write.run.DALEC
##' @title Function to generate generic model run script files
##' @author <unknown>
##' @import PEcAn.utils
#--------------------------------------------------------------------------------------------------#
write.run.DALEC <- function(settings){
  run.script.template = system.file("data", "run.template.DALEC", package="PEcAn.DALEC")
  run.text <- scan(file = run.script.template, 
                   what="character",sep='@', quote=NULL, quiet=TRUE)
  run.text  <- gsub('TMP', paste("/scratch/",Sys.getenv("USER"),sep=""), run.text)
  run.text  <- gsub('BINARY', settings$run$host$MODEL$binary, run.text)
  run.text <- gsub('OUTDIR', settings$run$host$outdir, run.text)
  runfile <- paste(settings$outdir, 'run', sep='')
  writeLines(run.text, con = runfile)
  if(settings$run$host$name == 'localhost') {
    system(paste('cp ', runfile, settings$run$host$rundir))
  }else{
    system(paste("rsync -outi ", runfile , ' ', settings$run$host$name, ":",
                 settings$run$host$rundir, sep = ''))
  }
}
#==================================================================================================#



####################################################################################################
### EOF.  End of R script file.            	
####################################################################################################
