PREFIX_XML <- '<?xml version="1.0"?>\n<!DOCTYPE config SYSTEM "ed.dtd">\n'

##### Generic functions #####
#returns a string representing a given number 
#left padded by zeros up to a given number of digits
left.pad.zeros <- function(num, digits){
  format_string <- paste('%',sprintf('0%.0f.0f',digits),sep='')
  return(sprintf(format_string, num))
}
rsync <- function(from, to){
  system(paste('rsync -outi', from, to, sep = ' '), intern=TRUE)
}
#returns an id representing a model run
#for use in model input files and indices
get.run.id <- function(run.type, index, trait='', pft.name=''){
  run.id <- paste(pft.name, run.type, trait, index, sep='')
  return(abbreviate.run.id.ED(run.id))
}
#As is the case with ED, input files must be <32 characters long.
#this function abbreviates run.ids for use in input files
abbreviate.run.id.ED <- function(run.id){
  run.id <- gsub('tundra.', '', run.id)
  run.id <- gsub('deciduous', 'decid', run.id)
  run.id <- gsub('evergreen', 'everg', run.id)
  run.id <- gsub('_', '', run.id)
  run.id <- gsub('root', 'rt', run.id)
  run.id <- gsub('water', 'h2o', run.id)
  run.id <- gsub('factor', '', run.id)
  run.id <- gsub('turnover', 'tnvr', run.id)
  run.id <- gsub('mortality', 'mort', run.id)
  run.id <- gsub('conductance', 'cond', run.id)
  run.id <- gsub('respiration', 'resp', run.id)
  run.id <- gsub('stomatalslope', 'stmslope', run.id)
  run.id <- gsub('nonlocaldispersal', 'nldisprs', run.id)
  run.id <- gsub('quantumefficiency', 'quantef', run.id)
  
  return(run.id)
} 
get.run.time <- function(){
  format(Sys.time(), '%Y.%m.%d')
}
listToXml <- function(item, tag){
  if(typeof(item)!='list')
    return(xmlNode(tag, item))
  xml <- xmlNode(tag)
  for(name in names(item)){
    xml <- append.xmlNode(xml, listToXml(item[[name]], name))
  }
  return(xml)
}
#Writes an xml and ED2IN config files for use with the Ecological Demography model.
#Requires a pft xml object, a list of trait values for a single model run,
#and the name of the file to create
write.config.ED <- function(pft, trait.samples, settings, outdir, run.id){
  xml <- listToXml(pft$constants, 'pft')
  for (trait in names(trait.samples)) {
    xml <- append.xmlNode(xml, xmlNode(trait, trait.samples[trait]))
  }
  config.header <- xmlNode("config")
  if ('config.header' %in% names(settings)){
    config.header <- listToXml(settings$config.header, 'config')
  } 
  xml <- append.xmlNode(config.header, xml)
  #c stands for config, abbreviated to work within ED's character limit
  xml.file.name <-paste('c.',run.id,sep='')  
  if(nchar(xml.file.name) >= 32) 
    stop(paste('The file name, "',xml.file.name,
            '" is too long and will cause your ED run to crash ',
            'if allowed to continue. '))
  saveXML(xml, file = paste(outdir, xml.file.name, sep=''), 
      indent=TRUE, prefix = PREFIX_XML)
  
  ed2in.text <- scan(file=pft$edin, 
      what="character",sep='@', quote=NULL, quiet=TRUE)
  ed2in.text <- gsub('OUTDIR', settings$run$host$outdir, ed2in.text)
  ed2in.text <- gsub('RUNTIME', get.run.time(), ed2in.text)
  ed2in.text <- gsub('ENSNAME', run.id, ed2in.text)
  ed2in.text <- gsub('USER', system('echo $USER', intern=TRUE), ed2in.text)
  ed2in.text <- gsub('CONFIGFILE', xml.file.name, ed2in.text)
  ed2in.text <- gsub('OUTFILE', paste('out', run.id, sep=''), ed2in.text)
  ed2in.text <- gsub('HISTFILE', paste('hist', run.id, sep=''), ed2in.text)
  ed2in.file.name <- paste('ED2INc.',run.id, sep='')
  writeLines(ed2in.text, con = paste(outdir, ed2in.file.name, sep=''))
  
  print(run.id)
}
#Performs model specific unit conversions on a a list of trait values,
#such as those provided to write.config
convert.samples.ED <- function(trait.samples){
  DEFAULT.LEAF.C <- 0.48
  ## convert SLA from kg leaf / m2 to kg C / m2
  if('SLA' %in% names(trait.samples)){
    if('leafC' %in% names(trait.samples))
      trait.samples[['SLA']] <- trait.samples[['SLA']] / trait.samples[['leafC']]
    else
      trait.samples[['SLA']] <- trait.samples[['SLA']] / DEFAULT.LEAF.C
  }
  
  ## convert leaf width / 1000
  if('leaf_width' %in% names(trait.samples)){
    trait.samples[['leaf_width']] <- trait.samples[['leaf_width']] / 1000.0
  }
  
  ## TODO: result[, c('mean','stat')] <- result[, c('mean','stat')] / 0.48 
  ## TODO: Vcmax -> Vm0
  
  return(trait.samples)
}


##### Ensemble functions #####
#Returns a matrix of pseudo random values assigned to traits over several model runs.
#given the number of model runs and a list of sample distributions for traits
#The model run is indexed first by model run, then by trait
get.ensemble.samples <- function(ensemble.size, samples) {
  #force as numeric for compatibility with Fortran code in halton()
  ensemble.size <- as.numeric(ensemble.size)
  
  halton.samples <- halton(n = ensemble.size, dim=length(samples))
  #force as a matrix in case length(samples)=1
  halton.samples <- as.matrix(halton.samples)
  
  ensemble.samples <- matrix(nrow = ensemble.size, ncol = length(samples))
  colnames(ensemble.samples) <- names(samples)
  for(ensemble.id in 1:ensemble.size) {
    for(trait.i in seq(samples)) {
      ensemble.samples[ensemble.id, trait.i] <- 
          quantile(samples[[trait.i]], halton.samples[ensemble.id, trait.i])
    }
  }
  return(ensemble.samples)
}
#Writes config files for use in meta-analysis and returns a list of run ids.
#Given a pft.xml object, a list of lists as supplied by get.sa.samples, 
#a name to distinguish the output files, and the directory to place the files.
write.ensemble.configs <- function(pft, ensemble.samples, host, outdir, settings,
    write.config = write.config.ED, convert.samples=convert.samples.ED){
  
  system(paste('ssh -T ', host$name, 
          ' "rm ', host$rundir, '/*', get.run.id('ENS', '', pft.name=pft.name), '*"', sep=''))
  for(ensemble.id in 1:nrow(ensemble.samples)) {
    run.id <- get.run.id('ENS', left.pad.zeros(ensemble.id, 5), 
        pft.name=pft$name)
    unlink(paste(outdir, '/*', run.id, '*', sep=''))
    write.config(pft, convert.samples(ensemble.samples[ensemble.id,]), 
        settings, outdir, run.id)
  }
  rsync(paste(outdir, '/*', get.run.id('ENS', '', pft.name=pft.name), '*', sep=''), 
      paste(host$name, ':', host$rundir,  sep=''))
}


##### Sensitivity analysis functions #####
#Returns a vector of quantiles specified by a given <quantiles> xml tag
get.quantiles <- function(quantiles.tag) {
  quantiles<-vector()
  if (!is.null(quantiles.tag$quantile)) {
    quantiles <- as.numeric(quantiles.tag[names(quantiles.tag)=='quantile'])
  }
  if (!is.null(quantiles.tag$sigma)){
    sigmas <- as.numeric(quantiles.tag[names(quantiles.tag)=='sigma'])
    quantiles <- append(quantiles, pnorm(1-sigmas))
  }
  if (length(quantiles) == 0) {
    quantiles <- 1-pnorm(-3:3) #default
  }
  if (!0.5 %in% quantiles) {
    quantiles <- append(quantiles, 0.5)
  }
  return(sort(quantiles))
}
#Returns a list of lists representing quantile values of trait distributions,
#given a list of sample distributions for traits and a list of quantiles
#The list is indexed first by trait, then by quantile
get.sa.samples <- function(samples, quantiles){
  sa.samples <- data.frame()
  for(trait in names(samples)){
    for(quantile in quantiles){
      sa.samples[as.character(round(quantile*100,3)), trait] <- quantile(samples[[trait]], quantile)
    }
  }
  return(sa.samples)
}
#Writes config files for use in sensitivity analysis, and returns a list of run ids.
#Given a pft.xml object, a list of lists as supplied by get.sa.samples, 
#a name to distinguish the output files, and the directory to place the files.
write.sa.configs <- function(pft, quantile.samples, host, outdir, settings, 
    write.config=write.config.ED, convert.samples=convert.samples.ED){
  MEDIAN <- '50'
  traits <- colnames(quantile.samples)
  
  system(paste('ssh -T ', host$name, 
          ' "rm ', host$rundir, '/*', get.run.id('SA', '', pft.name=pft.name), '*"', sep=''))
  
  median.samples <- quantile.samples[MEDIAN,]
  run.id <- get.run.id('SA', 'median', pft.name=pft$name)
  write.config(pft, convert.samples(median.samples), settings, outdir, run.id)
  
  for (trait in traits) {
    quantiles.str <- rownames(quantile.samples)
    for(quantile.str in quantiles.str) {
      if (quantile.str != MEDIAN) {
        quantile <- as.numeric(quantile.str)/100
        trait.samples <- median.samples
        trait.samples[trait] <- quantile.samples[quantile.str, trait]
        run.id <- get.run.id('SA', round(quantile,3), trait=trait, pft.name=pft$name)
        unlink(paste(outdir, '/*', run.id, '*', sep=''))
        write.config(pft, convert.samples(trait.samples), settings, outdir, run.id)
      }
    }
  }
  rsync(paste(outdir, '/*', get.run.id('SA', '', pft.name=pft.name), '*', sep=''), 
      paste(host$name, ':', host$rundir,  sep=''))
}

