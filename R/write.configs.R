if(interactive()){
  library(PECAn, lib.loc = '~/lib/R')
  source('~/pecan/R/pecan.config.constants.R')
}

PREFIX_XML <- '<?xml version="1.0"?>\n<!DOCTYPE config SYSTEM "ed.dtd">\n'
runIds <- list()
#TODO: sampleEnsemble should really not be a global variable
#it's instatiation depends upon the pft variable, which itself is a parameter to write.configs
#it should be passed as a parameter to write.configs, which would then modify its reference.
sampleEnsemble <- list()

#returns a string representing a given number 
#left padded by zeros up to a given number of digits
leftPadZeros <- function(num, digits){
    format_string <- paste('%',sprintf('0%.0f.0f',digits),sep='')
    return(sprintf(format_string, num))
}
 
configFileName <- function(outdir, runtype, runname, index, trait=''){
  runid <- paste(trait, runtype, runname, index, sep='')
  runIds <- c(runIds, runid)
  configfilename <- paste(outdir, '/c.', runid, sep='')
  return(configfilename)
}

writeEnsembleConfigs <- function(pft, ensembleSize, samples, runname, outdir){
  traits <- names(samples[[runname]])

  haltonSamples <- halton(n = ensembleSize, dim=length(traits))
  colnames(haltonSamples) <- traits

  for(ensembleId in 1:ensembleSize) {
    xml <- pft$PFT
    for (trait in traits) {
      sample <- quantile(samples[[runname]][[trait]], haltonSamples[ensembleId, trait])
      xml <- append.xmlNode(xml, xmlNode(trait, sample))
      sampleEnsemble[[runname]][ensembleId, trait] <- sample
    }
    xml <- append.xmlNode(pft$CONFIG, xml)
    file <- configFileName(outdir, runname, 'ENS', leftPadZeros(ensembleId, log10(ensembleSize)))
    print(file)
    saveXML(xml, file = file, indent=TRUE, prefix = PREFIX_XML)
  }
}

writeSAConfigs <- function(pft, Quantile.samples, runname, outdir){
  traits <- names(Quantile.samples[[runname]])
  xml.median <- pft$PFT
  for (trait in traits) {
    QuantilesStr <- names(Quantile.samples[[runname]][[trait]])
    median.i <- 0.5
    xml.median <- append.xmlNode(xml.median, xmlNode(trait, Quantile.samples[[trait]][median.i]))
    for(QuantileStr in QuantilesStr) {
      Quantile <- as.numeric(gsub('\\%', '', QuantileStr))/100 
      if (!is.na(Quantile) && Quantile != median.i) {
        xml.i <- append.xmlNode(pft$PFT, 
             xmlNode(trait, Quantile.samples[[runname]][[trait]][QuantileStr])) 
        for (otherTrait in traits[which(traits!=trait)]) {
          xml.i <- append.xmlNode(xml.i, xmlNode(otherTrait, Quantile.samples[[otherTrait]][median.i]))
        }
        xml.i <- append.xmlNode(pft$CONFIG, xml.i)
        file <- configFileName(outdir, 'SA', runname, round(Quantile,3), trait)
        print(file)
        saveXML(xml.i, file=file, indent=TRUE, prefix = PREFIX_XML)
      }
    }
  }
  xml.median <- append.xmlNode(pft$CONFIG, xml.median)
  file <- configFileName(outdir, 'SA', runname, 'median')
  print(file)
  saveXML(xml.median, file=file, indent=TRUE, prefix = PREFIX_XML)
}

write.configs <- function(pftName, ensembleSize, isSensitivityAnalysis, samples, 
                          Quantile.samples, outdir) {
  #KLUDGE: code assumes traits are the same throughout samples, and length(samples)>1
  traits <- names(samples[[1]])
  pft <- pecan.config.constants(pftName)
  
  sampleEnsemble <<- list(prior= matrix(nrow = ensembleSize, ncol = length(traits)), 
                          post=matrix(nrow = ensembleSize, ncol = length(traits)))
  colnames(sampleEnsemble[[1]]) <<- colnames(sampleEnsemble[[2]]) <<- traits
  
  for(runname in names(samples)) {
    writeEnsembleConfigs(pft, ensembleSize, samples, runname, outdir)

    if (isSensitivityAnalysis) {
      writeSAConfigs(pft, Quantile.samples, runname, outdir)
    }
  }
}

#Tests the write.configs function
test <- function(){
  pftName = 'ebifarm.c4crop'
  spstr <- query.bety.pft_species(pftName)

  trstr <- 
    "'mort2','cuticular_cond','dark_respiration_factor','plant_min_temp','growth_resp_factor',
    'leaf_turnover_rate','leaf_width','nonlocal_dispersal','q','root_respiration_factor',
    'root_turnover_rate','seedling_mortality','SLA_gC_per_m2','stomatal_slope','Vm_low_temp',
    'quantum_efficiency','f_labile','water_conductance','Vm0','r_fract','storage_turnover_rate', 
    'T'" #SLA_gC_per_m2 is converted to SLA in query.bety.priors
  priors <- query.bety.priors(pftName, trstr)
  #names(priors)[which(names(priors) %in% c('parama','paramb'))] <- c('a', 'b')

  i <- 1:10
  traits<-rownames(priors)
  trait.data <- query.bety.traits(spstr,traits)
  trait.mat <- data.frame(Vm0=trait.data$Vm0[i])

  prior.samps <- sapply(1:nrow(priors), function(x) do.call(pr.samp, priors[x,]))
  names(prior.samps) <- traits
  post.samps <- prior.samps
  for (trait in colnames(trait.mat)) post.samps[i,trait] <- trait.mat[i, trait]
  samples <- list(post=post.samps, prior=prior.samps)
  Quantiles<-1-pnorm(-3:3)
  calculate.quantiles <- function(x,samps) {
    quantile(samps[[x]], Quantiles)
  }
  Quantile.samples <- list(post  = lapply(traits, calculate.quantiles, post.samps),
                           prior = lapply(traits, calculate.quantiles, prior.samps))
  names(Quantile.samples$post) <- traits
  names(Quantile.samples$prior) <- traits
  tryCatch({
    write.configs(pftName = pftName, ensembleSize=10, isSensitivityAnalysis=TRUE, 
                  samples, Quantile.samples,  outdir='~/pecan/out')
  },
  error = function(ex) {
    print(ex)
    traceback()
  })
}

#Reloads the script
reload <- function(){
  rm(list=ls())
  source('~/pecan/R/write.configs.R')
}

retest <- function() {
  reload()
  test()
}
