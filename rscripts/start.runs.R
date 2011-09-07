library(XML)
if(interactive()){
  user <- Sys.getenv('USER')
  if(user == 'dlebauer'){
    settings.file = '~/pecan/settings.pavi.xml'
  } else if(user == 'davids14') {
    settings.file = '~/pecan/tundra.xml'
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

#Run model from user made bash script 
if(host$name == 'localhost') {
  system(paste('cd ', host$rundir, ';',
               settings$pecanDir, "bash/batch.jobs.sh", sep = ''))
}else{
  system(paste("echo 'cd ", host$rundir, "' | ",
               "cat - ", settings$pecanDir, "bash/batch.jobs.sh | ",
               'ssh -T ', host$name, sep = ''))
}
