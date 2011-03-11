
PREFIX_XML <- '<?xml version="1.0"?>\n<!DOCTYPE config SYSTEM "ed.dtd">\n'
run.ids <- list()

                                        #returns a string representing a given number 
                                        #left padded by zeros up to a given number of digits
lef.pad.zeros <- function(num, digits){

  format_string <- paste('%',sprintf('0%.0f.0f',digits),sep='')
  return(sprintf(format_string, num))
}


config.file.name <- function(outdir, runtype, runname, index, trait=''){

  runid <- paste(runname, runtype, trait, index, sep='')
  run.ids <<- c(run.ids, runid)
  config.file.name <- paste(outdir, '/c.', runid, sep='')
  return(config.file.name)
}

write.ensemble.configs <- function(pft, ensembleSize, samples, runname, outdir){
  traits <- names(samples[[runname]])
  sample.ensemble <- matrix(nrow = ensembleSize, ncol = length(traits))

  haltonSamples <- halton(n = ensembleSize, dim=length(traits))
  colnames(haltonSamples) <- traits

  for(ensembleId in 1:ensembleSize) {
    xml <- pft$PFT
    for (trait in traits) {
      sample <- quantile(samples[[runname]][[trait]], haltonSamples[ensembleId, trait])
      xml <- append.xmlNode(xml, xmlNode(trait, sample))
      sample.ensemble[ensembleId, trait] <- sample
    }
    xml <- append.xmlNode(pft$CONFIG, xml)
    file <- config.file.name(outdir, runname, 'ENS', lef.pad.zeros(ensembleId, log10(ensembleSize)))
    print(file)
    saveXML(xml, file = file, indent=TRUE, prefix = PREFIX_XML)
  }
  return(sample.ensemble)
}

write.sa.configs <- function(pft, Quantile.samples, runname, outdir){
  traits <- names(Quantile.samples[[runname]])
  xml.median <- pft$PFT
  for (trait in traits) {
    QuantilesStr <- names(Quantile.samples[[runname]][[trait]])
    median.i <- 0.5
    xml.median <- append.xmlNode(xml.median, xmlNode(trait, Quantile.samples[[trait]][median.i]))
    for(QuantileStr in QuantilesStr) {
      Quantile <- as.numeric(gsub('\\%', '',QuantileStr))/100
      if (!is.na(Quantile) && Quantile != median.i) {
        xml.i <- append.xmlNode(pft$PFT, xmlNode(trait, Quantile.samples[[runname]][[trait]][QuantileStr])) 
        for (otherTrait in traits[which(traits!=trait)]) {
          xml.i <- append.xmlNode(xml.i, xmlNode(otherTrait, Quantile.samples[[otherTrait]][median.i]))
        }
        xml.i <- append.xmlNode(pft$CONFIG, xml.i)
        file <- config.file.name(outdir, 'SA', runname, round(Quantile,3), trait)
        print(file)
        saveXML(xml.i, file=file, indent=TRUE, prefix = PREFIX_XML)
      }
    }
  }
  xml.median <- append.xmlNode(pft$CONFIG, xml.median)
  file <- config.file.name(outdir, 'SA', runname, 'median')
  print(file)
  saveXML(xml.median, file=file, indent=TRUE, prefix = PREFIX_XML)
}

write.configs <- function(pftName, ensembleSize, isSensitivityAnalysis, samples, 
                          Quantile.samples, outdir) {
                                        #KLUDGE: code assumes traits are the same throughout samples, and length(samples)>=1
  traits <- names(samples[[1]])

  pftXml <- pecan.config.constants(pftName)
  
  sample.ensemble <- list()
  
  for(runname in c('prior','post')) {
    sample.ensemble.matrix <- list(write.ensemble.configs(pftXml, ensembleSize, samples, runname, outdir))
    names(sample.ensemble.matrix) <- runname
    sample.ensemble <- append(sample.ensemble, list(runname=sample.ensemble.matrix))

    if (isSensitivityAnalysis) {
      write.sa.configs(pftXml, Quantile.samples, runname, outdir)
    }
  }
  save(sample.ensemble, file = paste(outdir, 'sample.ensemble.RData', sep=''))
}
