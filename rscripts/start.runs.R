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

model.host     <-  unlist(xpathApply(settings.xml, '//run//host//name', xmlValue))
model.rundir   <-  unlist(xpathApply(settings.xml, '//run//host//rundir', xmlValue))
model.outdir   <-  unlist(xpathApply(settings.xml, '//run//host//outdir', xmlValue))

system(paste('cd', outdir))
system(paste('rsync -outi ', outdir, 'configs.tgz ', model.host, ':', model.outdir, sep = ''))
system(paste('ssh -T  <', pecanDir, '/bash/write.namelists.sh', model.outdir, '; wait', sep = ''))

## move config files to run directory
system(paste('ssh ', model.host, ' rsync -outi ', model.outdir, '*c.* ',  model.rundir, sep = '')) 
system(paste('ssh -T', model.host,  '< ', pecanDir, '/bash/batch.jobs.sh', sep = ''))

##after runs complete, run pecan.SA.sh
