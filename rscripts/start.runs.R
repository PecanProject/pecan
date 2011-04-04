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

settings.xml <- xmlParse(settings.file)
settings <- xmlToList(settings.xml)

outdir       <- settings$outdir
pecanDir    <- settings$pecanDir

run.host     <-  unlist(xpathApply(settings.xml, '//run//host//hostname', xmlValue))
model.rundir <-  unlist(xpathApply(settings.xml, '//run//host//model.rundir', xmlValue))
model.outdir <-  unlist(xpathApply(settings.xml, '//run//host//model.outdir', xmlValue))

system(paste('cd', outdir))
system(paste('rsync -outi configs.tgz ', run.host, ':', model.outdir, sep = ''))
system(paste('ssh -T ebi-cluster:', model.outdir, ' < ' ,  pecanDir, '/bash/pecan.ed2in.create.sh', sep = ''))
wait
## unzip config files, set env vars, run ED ensemble, 
ssh -T ebi-cluster < $PECANDIR/bash/pecan.ed.batchjobs.sh
#next: wait for runs to compled, can use $edstat
#then  pecan.SA.sh
