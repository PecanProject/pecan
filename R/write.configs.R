PREFIX_XML <- '<?xml version="1.0"?>\n<!DOCTYPE config SYSTEM "ed.dtd">\n'

const <- pecan.config.constants(pft)
pftXml <- const$PFT
configXml <- const$CONFIG

runIds <- list()
sampleEnsemble <- list(prior= matrix(nrow = M, ncol = length(traits)), post=matrix(nrow = M, ncol = length(traits)))
colnames(sampleEnsemble[[1]]) <- colnames(sampleEnsemble[[2]]) <- traits

#returns a string representing a given number 
#left padded by zeros up to a given number of digits
leftPadZeros <- function(num, digits){
    format_string <- paste('%',sprintf('0%.0f.0f',digits),sep='')
    return(sprintf(format_string, seq(1, ensemble_size)))
}
 
configFileName <- function(outdir, runtype, runname, index, trait=''){
  runid <- paste(trait, runtype, runname, index, sep='')
  runIds <- c(runIds, runid)
  configfilename <- paste(outdir, 'c.', runid, sep='')
  return(configfilename)
}


writeENSXml <- function(outdir, runname, ensembleId, pftXml, traits, haltonSamples, samples,  
                           prefixXml = PREFIX_XML){
  pftXml.i <- pftXml
  for (trait in traits) {
    sample <- quantile(samples[[runname]][,trait], haltonSamples[ensembleId, trait])
    pftXml.i <- append.xmlNode(pftXml.i, xmlNode(trait, sample))
    sampleEnsemble[[runname]][ensembleId, trait] <- sample
  }
  configXml.i <- append.xmlNode(configXml, pftXml.i)
  file <- configFileName(outdir, runname, 'ENS', ensembleId)
  saveXML(configXml.i, file = file, indent=TRUE, prefix = prefixXml)
}

writeSAXml <- function(outdir, runname, pftXml, traits, Quantile.samples, prefixXml = PREFIX_XML){  
  pftXml.median <- pftXml
  for (trait in traits) {
    Quantiles <- as.numeric(gsub('\\%', '',names(Quantile.samples[[runname]][trait])))
    median.i <- which(Quantiles == 0.5)
    pftXml.median <- append.xmlNode(pftXml.median, xmlNode(trait, Quantile.samples[[trait]][median.i]))
    for(Quantile in seq(Quantiles)) {
      if (Quantile !=median.i) {
        pftXml.i <- append.xmlNode(pftXml, xmlNode(trait, Quantile.samples[[runname]][[trait]][Quantile.i])) 
        for (otherTrait in traits[which(traits!=trait)]) {
          pftXml.i <- append.xmlNode(pftXml.i, xmlNode(otherTrait, Quantile.samples[[otherTrait]][median.i]))
        }
        configXml.i <- append.xmlNode(configXml, pftXml.i)
        file <- configFileName(outdir, 'SA', runname, Quantile)
        saveXML(configXml.i, file=file, indent=TRUE, prefix = prefixXml)
      }
    }
  }
  configXml.median <- append.xmlNode(configXml, pftXml.median)
  file <- configFileName(outdir, 'SA', runname, 'median')
  saveXML(configXml.median, file=file, indent=TRUE, prefix = prefixXml)
}



write.configs <- function(ensemble_size, sensitivity_analysis, pftXml, samples,
                          Quantile.samples, outdir, traits, runname) {

  if(ensemble_size > 0 ) { # write files for ensemble
    haltonSamples <- halton(n = ensemble_size, dim=length(traits))
    colnames(haltonSamples) <- traits
    for(ensembleId in seq(ensemble_size)) {
      runName <- leftPadZeros(zero.run, log10(ensemble_size))
      writeENSXml(outdir, runname, ensembleId, pftXml, traits, samples, samps)
    }
  }
  if (sensitivity_analysis) {
    writeSAXml(outdir, runname, pft, traits, Quantile, Quantile.samples[[runname]])
  }   
}

write.configs(ensemble_size=10, sensitivity_analysis=TRUE, pftXml=PFT, samples,
                          Quantile.samples,  outdir, Quantiles, runname='post')
