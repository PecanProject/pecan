#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
library(XML)
if(interactive()){
  user <- Sys.getenv('USER')
  if(user == 'dlebauer'){
    settings.file <- '~/in/ebifarm/fast/ebifarm.pavi.xml'
  if(user == 'djaiswal'){
    settings.file <- '~/in/ebifarm/fast/ebifarm.pavi.xml'
  } else if(user == 'davids14') {
    settings.file <- '~/pecan/tundra.xml'
  } else {
    paste('please specify settings file in meta.analysis.R')
  }
  }
} else {
  settings.file <- Sys.getenv("PECANSETTINGS")
}

settings.xml <- xmlParse(settings.file)
settings <- xmlToList(settings.xml)
host     <-  settings$run$host
if(!is.null(settings$Rlib)){ .libPaths(settings$Rlib)} 
library(PEcAn)


## Priority only needs to be set for very big jobs as with ED2
## priority can be NULL in settings, if null,
## only batch.jobs.sh is required to exist
if(is.null(settings$run$priority)){
  batch.jobs.script <- "bash/batch.jobs.sh"
} else if (as.numeric(settings$run$priority) < 0) {
  file.copy("bash/batch.jobs.sh", "bash/batch.jobs.lowp.sh", overwrite = TRUE)
  ## set priority 
  system(paste("sed -i 's/\"qsub/\"qsub\ -p\ ", settings$run$priority,
               "/g' bash/batch.jobs.lowp.sh", sep = ''))
  batch.jobs.script <- "bash/batch.jobs.lowp.sh"
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
               "cat - ", settings$pecanDir, batch.jobs.script, " | ",
               'ssh -T ', host$name, sep = ''))
}
