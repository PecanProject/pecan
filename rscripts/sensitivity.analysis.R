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
  ## settings.file <- commandArgs(trailingOnly=TRUE)
} 

settings.xml <- xmlParse(settings.file)
settings <- xmlToList(settings.xml)

if(!is.null(settings$Rlib)){ .libPaths(settings$Rlib)} 
library(PECAn)

outdir <- settings$outdir
host<- settings$run$host
load(paste(outdir, 'samples.Rdata', sep=''))

rsync(from = paste(settings$pecanDir, 'rscripts/read.output.R ', sep = ''),
      to = paste(host$name, ':',host$outdir, sep = ''))
system(paste("ssh -T", host$name, "'", "cd", host$outdir, "; R --vanilla < read.output.R'"))

                                        #ssh(host$name, 'cd ', host$outdir, run.time, '/ ; R --vanilla ',
                                        #    args=paste('<', settings$pecanDir, '/rscripts/read.output.R',sep=''))
rsync(from = paste(host$name, ':', host$outdir, 'output.Rdata', sep=''),
      to = paste(settings$outdir))
load(paste(outdir, 'output.Rdata', sep=''))

for(pft in settings$pfts){
  traits <- names(trait.samples[[pft$name]])
  quantiles.str <- rownames(sa.samples[[pft$name]])
  quantiles.str <- quantiles.str[which(quantiles.str != '50')]
  quantiles <- as.numeric(quantiles.str)/100
                                        #ensemble.output <- read.ensemble.output(settings$ensemble$size, outdir, pft.name=pft$name)
  
  if('sensitivity.analysis' %in% names(settings)) {
    sensitivity.results <- sensitivity.analysis(trait.samples = trait.samples[[pft$name]],
                                                sa.samples = sa.samples[[pft$name]],
                                                sa.output = sa.agb[[pft$name]],
                                                outdir = pft$outdir)
    plot.sensitivities(sensitivity.results[['sensitivity.plot.inputs']], outdir = pft$outdir)  
    plot.variance.decomposition(sensitivity.results[[variance.decomposition.plot.inputs]], outdir = pft$outdir)
  }
}
