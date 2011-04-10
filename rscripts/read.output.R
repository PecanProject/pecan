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

print('Enter runtime: ')
run.time <- readline()

settings.xml <- xmlParse(settings.file)
settings <- xmlToList(settings.xml)
host<- settings$run$host
outdir<-settings$outdir

if(!is.null(settings$Rlib)){ .libPaths(settings$Rlib)} 
library(PECAn)

pft <- settings$pfts[[1]]

#retrieve output from host machine
system(paste('rsync -outi ', 
       host$name, ':', host$outdir, run.time, '/* ',
       pft$outdir, sep = ''))

load(paste(outdir, 'ensemble.samples.Rdata', sep=''))
ensemble.output <- read.ensemble.output(settings$ensemble$size, pft$outdir)
save(ensemble.output, file = paste(outdir, 'ensemble.output.Rdata', sep = ''))

if('sensitivity.analysis' %in% names(settings)) {
  load(paste(outdir, 'sa.samples.Rdata', sep=''))
  traits <- names(sa.samples[[pft$name]])
  quantiles.str <- names(sa.samples[[pft$name]][[1]])
  quantiles.str <- quantiles.str[which(quantiles.str != '50%')]
  quantiles <- as.numeric(gsub('\\%', '',quantiles.str))/100
  sa.agb <- read.sa.output(traits, quantiles, pft$outdir)
  save(sa.agb, file = paste(outdir, 'sa.output.Rdata', sep = ''))
}