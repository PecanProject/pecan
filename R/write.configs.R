PREFIX_XML = '<?xml version="1.0"?>\n<!DOCTYPE config SYSTEM "ed.dtd">\n'

#returns a string representing a given number 
#left padded by zeros up to a given number of digits
leftPadZeros = function(num, digits){
    format_string = paste('%',sprintf('0%.0f.0f',digits),sep='')
    return(sprintf(format_string, seq(1, ensemble_size)))
}
registerFileName = function(type, typeIndex, id){
  file = paste(outdir, '/config.', type, quantile, '.xml', sep='')
  filenames[[typeIndex]][id] = file
  return(file)
}
writeSampleXml = function(file, pft, traits, samples, samplePosteriors, sampleEnsemble, 
      prefixXml = PREFIX_XML){
  pftXml = pft
  for (trait in traits) {
    sample = quantile(samplePosteriors[,trait], samples[trait])
    pftXml = append.xmlNode(pftXml, xmlNode(trait, sample)
    sampleEnsemble[k] = sample
  }
  configXml = append.xmlNode(config, pftXml)
  saveXML(configXml, file = file, indent=TRUE, prefix = prefix)
}
#David: are these function names appropriate?
writeQuantileXml = function(file, pft, traits, quantile, qdata, prefixXml = PREFIX_XML){
	pftXml = pft
	for (trait in traits) {
	  pftXml = append.xmlNode(pftXml, xmlNode(trait, qdata[[trait]][quantile]))
	  for (otherTrait in traits[which(traits!=trait)]) {
	    pftXml = append.xmlNode(pftXml, xmlNode(otherTrait, qdata[[otherTrait]][n.mean]))
	    #TODO: define n.mean
	  }
	}
	configXml = append.xmlNode(config, pftXml)
	saveXML(configXml, file=file, indent=TRUE, prefix = PREFIX_XML)
}


write.configs <- function(ensemble_size, sensitivity_analysis, pft, ens.samps, quantile.samples, outdir, quantiles, traits) {

  priors$distn[priors$distn=='weib'] = 'weibull'
  
  for (quantile in quantiles) {
    writeQuantileXml(registerFileName('prior', 'priorSA', quantile),
        pft, traits, quantile, qprior)
    writeQuantileXml(registerFileName('post', 'postSA', quantile),
        pft, traits, quantile, qpost)
  }

  if(ensemble_size > 0 ) { # write files for ensemble

    #foo <- lapply(ens.samps, function(x) subset(x, subset = )
    for(run in seq(ensemble_size)) {
      samples = halton(n =length(traits) dim = 1)
      runName = leftPadZeros(zero.run, log10(ensemble_size))
      writeSampleXml(registerFileName('postsamp','post.ensemble',runName), 
          pft, traits, samples, post.samps, ens.samps[['post']][run,])
      writeSampleXml(registerFileName('priorsamp','prior.ensemble', runName), 
          pft, traits, samples, prior.samps, ens.samps[['prior']][run,])
    }
  }
  if (sensitivity_analysis) {
  ## Create the config for median runs
    for(runname in names(quantile.samples)) {
      PFTm <- PFT
      for (tri in traits) { 
        PFTm <- append.xmlNode(PFTm, xmlNode(tri, quantile.samples[[runname]][[tri]]))
      }
      CONFIGm <- append.xmlNode(CONFIG, PFTm)
      file <- paste(outdir, "/config.priormeans.xml", sep = '')
      filenames[['priormeans']] <- file
      saveXML(CONFIGm, file = file, indent = TRUE, prefix = '<?xml version=\"1.0\"?>\n<!DOCTYPE config SYSTEM \"ed.dtd\">\n')
      
  }
}
