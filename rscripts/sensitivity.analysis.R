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

if(!is.null(settings$Rlib)){ .libPaths(settings$Rlib)} 
library(PECAn)

pft <- settings$pfts[[1]]
outdir <- settings$outdir

load(paste(outdir, 'trait.samples.Rdata', sep=''))
load(paste(outdir, 'sa.output.Rdata', sep=''))
load(paste(outdir, 'sa.samples.Rdata', sep=''))
#load(paste(outdir,'ensemble.output.Rdata', sep='')



sensitivity.analysis <- function(trait.samples, sa.samples, sa.output){
  traits <- names(trait.samples)
  sa.splinefuns <- sapply(traits, function(trait) sa.splinefun(sa.samples[[trait]], sa.output[[trait]]))
  
  
  saplots <-  lapply(traits, function(x) sensitivity.plot(sa.samples[[x]], sa.splinefuns[[x]], x))
  pdf('sensitivity.analysis.pdf', height = 12, width = 20)
  saplots#left='Aboveground Biomass', main='Parameter Sensitivity', nrow=3,ncol=5) 
  dev.off()
  
  spline.estimates <- sapply(traits, function(trait) sa.splinefuns[[trait]](trait.samples[[trait]]))
  spline.estimates <- zero.truncate(spline.estimates)
  sensitivities <- sapply(traits, function(trait) get.sensitivity(trait.samples[[trait]], sa.splinefuns[[trait]]))
  elasticities <- sapply(traits, 
      function(trait) get.elasticity(sensitivities[[trait]], trait.samples[[trait]], spline.estimates[,trait]))
  explained.variances <- get.explained.variances(spline.estimates)
  
  #TODO: move unit conversions to their own method, called before sensitivity analysis 
  if('Vm_low_temp' %in% traits)
    trait.samples[which(traits == 'Vm_low_temp')] <- trait.samples[which(traits == 'Vm_low_temp')] + 273.15
  coef.vars <- sapply(trait.samples, get.coef.var)
  
  pdf('variancedecomposition.pdf', width = 12, height = 8)
  grid.arrange(qplot(names(explained.variances), coef.vars) + coord_flip(),
      qplot(1:length(explained.variances), elasticities) + coord_flip(),
      qplot(1:length(explained.variances), explained.variances) + coord_flip(),
      ncol = 3)
  dev.off()
}

sensitivity.analysis(trait.samples[[1]], sa.samples[[1]], sa.agb)
