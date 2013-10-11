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

convert.samples.dalec <- function(trait.samples){
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
write.config.dalec <- function(defaults, trait.values, settings, run.id){
   
  ### PARAMETERS
  cmdFlags = ""
  params <- convert.samples.dalec(trait.values)
  for(i in 1:length(params)){
    cmdFlags <- paste(cmdFlags," -",names(params)[i]," ",params[[i]],sep="")
  }
  config.file.name <- paste('CONFIG.',run.id, sep='')
  writeLines(cmdFlags, con = paste(outdir, config.file.name, sep=''))
      
    ### Display info to the console.
    print(run.id)
}
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##'
##' @name write.run.dalec
##' @title Function to generate generic model run script files
##' @author <unknown>
##' @import PEcAn.utils
#--------------------------------------------------------------------------------------------------#
write.run.dalec <- function(settings){
  run.script.template = system.file("data", "run.template.MODEL", package="PEcAn.MODEL")
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
