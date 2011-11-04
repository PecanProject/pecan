library(XML)
if(interactive()){
  user <- Sys.getenv('USER')
  if(user == 'dlebauer'){
    settings.file <- '~/in/ebifarm/prior/ebifarm.pavi.xml'
  } else if(user == 'davids14') {
    settings.file <- '~/pecan/tundra.xml'
  } else {
    paste('please specify settings file in meta.analysis.R')
  }
} else {
  settings.file <- Sys.getenv("PECANSETTINGS")
}

settings.xml <- xmlParse(settings.file)
settings <- xmlToList(settings.xml)
host     <-  settings$run$host
if(!is.null(settings$Rlib)){ .libPaths(settings$Rlib)} 
library(PECAn)

if(is.null(settings$run$priority)){
  batch.jobs.script <- "bash/batch.jobs.sh"
} else if (as.numeric(settings$run$priority) < 0) {
  file.copy("bash/batch.jobs.sh", "bash/batch.jobs.lowp.sh", overwrite = TRUE)
  ## set priority 
  system(paste("sed -i 's/\"qsub/\"qsub\ -p\ ", settings$run$priority,
               "/g' bash/batch.jobs.lowp.sh", sep = ''))
  batch.jobs.script <- "bash/batch.jobs.lowp.sh"
} else {
  stop("need admin rights to set higher priority")
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
