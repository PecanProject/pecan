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

pft <- settings$pfts[[1]]
outdir <- settings$outdir
host<- settings$run$host

#retrieve output from host machine
system(paste('rsync -outi ', 
        host$name, ':', host$outdir, run.time, '/* ',
        pft$outdir, sep = ''))

#ensemble.output <- read.ensemble.output(settings$ensemble$size, pft$outdir)

if('sensitivity.analysis' %in% names(settings)) {
  load(paste(outdir, 'trait.samples.Rdata', sep=''))
  load(paste(outdir, 'sa.samples.Rdata', sep=''))
  traits <- names(trait.samples[[pft$name]])
  quantiles.str <- rownames(sa.samples[[pft$name]])
  quantiles.str <- quantiles.str[which(quantiles.str != '50')]
  quantiles <- as.numeric(quantiles.str)/100
  sa.agb <- read.sa.output(traits, quantiles, pft$outdir)
  sensitivity.analysis(trait.samples[[pft$name]], sa.samples[[pft$name]], sa.agb)
}

