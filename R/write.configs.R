
PREFIX_XML <- '<?xml version="1.0"?>\n<!DOCTYPE config SYSTEM "ed.dtd">\n'

##### Generic functions #####
#returns a string representing a given number 
#left padded by zeros up to a given number of digits
left.pad.zeros <- function(num, digits){
  format_string <- paste('%',sprintf('0%.0f.0f',digits),sep='')
  return(sprintf(format_string, num))
}
#returns an id representing a model run
#for use in model input files and indices
get.run.id <- function(run.type, index, trait='', pft.name=''){
  return(paste(pft.name, run.type, trait, index, sep=''))
}
#returns a standardized name for a model input file
get.file.name <- function(outdir, run.id){
  return(paste(outdir,'/c.',run.id,sep=''))
}
#Writes an xml based input file for use with the Ecological Demography model.
#Requires a pft.xml object, a list of trait values for a *single* model run,
#and the name of the file to create
write.config.ED <- function(pft.xml, trait.samples, file.name){
  traits <- names(trait.samples)
  xml <- pft.xml$PFT
  for (trait in traits) {
    xml <- append.xmlNode(xml, xmlNode(trait, trait.samples[trait]))
  }
  xml <- append.xmlNode(pft.xml$CONFIG, xml)
  saveXML(xml, file = file.name, indent=TRUE, prefix = PREFIX_XML)
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
write.ensemble.configs <- function(pft.xml, ensemble.samples, outdir, pft.name='',
    write.config = write.config.ED, convert.samples=convert.samples.ED){

  run.ids<-list()
  for(ensemble.id in 1:nrow(ensemble.samples)) {
    run.id <- get.run.id('ENS', left.pad.zeros(ensemble.id, 5), 
                         pft.name=pft.name)
    run.ids <- append(run.ids, run.id)
    file.name <- get.file.name(outdir, run.id)
    write.config(pft.xml, convert.samples(ensemble.samples[ensemble.id,]), file.name)
    run.ids<-append(run.ids, file.name)
    print(file.name)
  }
  return(run.ids)
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
  sa.samples <- lapply(names(samples), function(trait) quantile(samples[[trait]], quantiles))
  names(sa.samples) <- names(samples)
  return(sa.samples)
}
#Writes config files for use in sensitivity analysis, and returns a list of run ids.
#Given a pft.xml object, a list of lists as supplied by get.sa.samples, 
#a name to distinguish the output files, and the directory to place the files.
write.sa.configs <- function(pft.xml, quantile.samples, outdir, pft.name='',
                             write.config=write.config.ED, convert.samples=convert.samples.ED){
  MEDIAN <- '50%'
  traits <- names(quantile.samples)
  
  median.samples <- lapply(traits, 
      function(trait) quantile.samples[[trait]][[MEDIAN]])
  names(median.samples) <- traits
  run.id <- get.run.id('SA', 'median', pft.name=pft.name)
  file.name <- get.file.name(outdir, run.id)
  write.config(pft.xml, convert.samples(median.samples), file.name)
  print(file.name)

  run.ids <- list(run.id)
  for (trait in traits) {
    quantiles.str <- names(quantile.samples[[trait]])
    for(quantile.str in quantiles.str) {
      if (quantile.str != MEDIAN) {
        quantile <- as.numeric(gsub('\\%', '',quantile.str))/100
        trait.samples <- median.samples
        trait.samples[trait] <- quantile.samples[[trait]][quantile.str]
        run.id <- get.run.id('SA', round(quantile,3), trait=trait, pft.name=pft.name)
        file.name <- get.file.name(outdir, run.id)
        write.config(pft.xml, convert.samples(trait.samples), get.file.name(outdir, run.id))
        print(file.name)
        run.ids <- append(run.ids, run.id)
      }
    }
  }
  return(run.ids)
}

