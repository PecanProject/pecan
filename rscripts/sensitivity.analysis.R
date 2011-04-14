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

if(!is.null(settings$Rlib)){ .libPaths(settings$Rlib)} 
library(PECAn)

outdir <- settings$outdir
host<- settings$run$host
load(paste(outdir, 'trait.samples.Rdata', sep=''))
load(paste(outdir, 'sa.samples.Rdata', sep=''))

for(pft in settings$pfts){
  #ensemble.output <- read.ensemble.output(settings$ensemble$size, outdir, pft.name=pft$name)
  
  if('sensitivity.analysis' %in% names(settings)) {
    traits <- names(trait.samples[[pft$name]])
    quantiles.str <- rownames(sa.samples[[pft$name]])
    quantiles.str <- quantiles.str[which(quantiles.str != '50')]
    quantiles <- as.numeric(quantiles.str)/100
    sa.agb <- read.sa.output(traits, quantiles, host, pft$outdir, run.time, pft.name=pft$name)
    sensitivity.analysis(trait.samples[[pft$name]], sa.samples[[pft$name]], sa.agb, pft$outdir)
  }
}

