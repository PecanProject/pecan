if(interactive()){
  library(PECAn, lib.loc = '~/lib/R')
  source('~/pecan/R/pecan.config.constants.R')
}

PREFIX_XML <- '<?xml version="1.0"?>\n<!DOCTYPE config SYSTEM "ed.dtd">\n'
runIds <- list()
#sampleEnsemble should really not be a global variable
#it's instatiation depends upon the pft variable, which itself is a parameter to write.configs
#it should be passed as a parameter to write.configs which would then modify its reference.
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
  configfilename <- paste(outdir, 'c.', runid, sep='')
  return(configfilename)
}

writeEnsembleConfigs <- function(pft, ensembleSize, samples, sampleEnsemble, runname, outdir){
  traits <- colnames(samples[[runname]])

  haltonSamples <- halton(n = ensembleSize, dim=length(traits))
  colnames(haltonSamples) <- traits

  for(ensembleId in seq(ensembleSize)) {
    xml <- pft$PFT
    for (trait in traits) {
      sample <- quantile(samples[[runname]][,trait], haltonSamples[ensembleId, trait])
      xml <- append.xmlNode(xml, xmlNode(trait, sample))
      sampleEnsemble[[runname]][ensembleId, trait] <- sample
    }
    xml <- append.xmlNode(pft$CONFIG, xml)
    file <- configFileName(outdir, runname, 'ENS', leftPadZeros(ensembleId, log10(ensemble_size)))
    saveXML(xml, file = file, indent=TRUE, prefix = PREFIX_XML)
  }
}

writeSAConfigs <- function(pft, Quantile.samples, runname, outdir){
  traits <- colnames(Quantile.samples[[runname]])

  xml.median <- pft$PFT
  for (trait in traits) {
    Quantiles <- as.numeric(gsub('\\%', '',names(Quantile.samples[[runname]][trait])))
    median.i <- which(Quantiles == 0.5)
    xml.median <- append.xmlNode(xml.median, xmlNode(trait, Quantile.samples[[trait]][median.i]))
    for(Quantile in seq(Quantiles)) {
      if (Quantile !=median.i) {
        xml.i <- append.xmlNode(pft$PFT, xmlNode(trait, Quantile.samples[[runname]][[trait]][Quantile.i])) 
        for (otherTrait in traits[which(traits!=trait)]) {
          xml.i <- append.xmlNode(xml.i, xmlNode(otherTrait, Quantile.samples[[otherTrait]][median.i]))
        }
        xml.i <- append.xmlNode(pft$CONFIG, xml.i)
        file <- configFileName(outdir, 'SA', runname, Quantile)
        saveXML(xml.i, file=file, indent=TRUE, prefix = PREFIX_XML)
      }
    }
  }
  xml.median <- append.xmlNode(pft$CONFIG, xml.median)
  file <- configFileName(outdir, 'SA', runname, 'median')
  saveXML(xml.median, file=file, indent=TRUE, prefix = prefixXml)
}

write.configs <- function(pftName, ensembleSize, isSensitivityAnalysis, samples, 
                          Quantile.samples, outdir) {
  traits <- colnames(samples[[runname]])

  pft <- pecan.config.constants(pftName)
  sampleEnsemble <- list(prior= matrix(nrow = ensembleSize, ncol = length(traits)), 
                         post=matrix(nrow = ensembleSize, ncol = length(traits)))
  colnames(sampleEnsemble[[1]]) <- colnames(sampleEnsemble[[2]]) <- traits
  
  for(runname in samples) {
    writeEnsembleConfigs(pft, ensembleSize, samples, runname)

    if (isSensitivityAnalysis) {
      writeSAConfigs(pft, Quantile.samples, runname, outdir)
    }
  }
}

#Tests the write.configs function
test <- function(){
  pftName = 'ebifarm.c4crop'
  trstr <- 
    "'mort2','cuticular_cond','dark_respiration_factor','plant_min_temp','growth_resp_factor',
    'leaf_turnover_rate','leaf_width','nonlocal_dispersal','q','root_respiration_factor',
    'root_turnover_rate','seedling_mortality','SLA_gC_per_m2','stomatal_slope','Vm_low_temp',
    'quantum_efficiency','f_labile','water_conductance','Vm0','r_fract','storage_turnover_rate', 
    'T'" #SLA_gC_per_m2 is converted to SLA in query.bety.priors
  priors <- query.bety.priors(pftName, trstr)

  prior.samps <- sapply(1:nrow(priors), function(x) do.call(pr.samp, priors[x,]))
  post.samps <- prior.samps
  traits<-colnames(post.samps)

  Quantiles<-1-pnorm(-3:3)
  calculate.quantiles <- function(x,samps, quantiles) {
    quantile(samps[,x], quantiles)
  }
  Quantile.samples <- list(post  = lapply(traits, calculate.quantiles, post.samps, quantiles),
                           prior = lapply(traits, calculate.quantiles, prior.samps, quantiles))
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
  do.call(rm, as.list(ls()))
  source('~/pecan/R/write.configs.R')
}

retest <- function() {
  reload()
  test()
}
