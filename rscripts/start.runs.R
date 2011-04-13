library(XML)
if(interactive()){
  user <- system('echo $USER', intern = TRUE)
  if(user == 'dlebauer'){
    settings.file = '~/pecan/settings.pavi.xml'
  } else if(user == 'davids14') {
    settings.file = '~/pecan/tundra.xml'
  } else {
    paste('please specify settings file in meta.analysis.R')
  }
} else {
  settings.file <- system("echo $PECANSETTINGS", intern = TRUE)
}

library(PECAn, lib.loc='~/lib/R')
settings.xml <- xmlParse(settings.file)
settings <- xmlToList(settings.xml)
host     <-  settings$run$host

#Make outdirectory
system(paste('ssh -T ', host$name, 
             ' "mkdir ', host$outdir, get.run.time(), '"',sep=''))
#Run model from user made bash script 
system(paste("echo 'cd ", host$rundir, "' | ",
             "cat - ", settings$pecanDir, "bash/batch.jobs.sh | ",
             'ssh -T ', host$name, sep = ''))
