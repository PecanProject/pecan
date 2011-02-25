write.configs <- function(ensemble_size, sensitivity_analysis, pft, ens.samps, quantile.samples, outdir, quantiles) {

  priors$distn[priors$distn=='weib'] <- 'weibull'

  const <- pecan.config.constants(pft)
  PFT <- const$PFT
  CONFIG <- const$CONFIG
  traits <-  colnames(ens.samps[[1]])
  filenames <- list()

  if(ensemble_size > 0 ) { # write files for ensemble
    ens.sequence <- seq(1, ensemble_size)
    zeros.ens <- sprintf("%04.0f", ens.sequence) #"%05.0f" if > 10^5 runs, etc.

    samps <- halton(n = ensemble_size, dim = length(traits)) 
    colnames(samps) <-  traits


    foo <- lapply(ens.samps, function(x) subset(x, subset = )
                

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
