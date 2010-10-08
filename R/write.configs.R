write.configs <- function(M, pft, prior.samps, post.samps) {

  const <- pecan.config.constants(pft)
  PFT <- const$PFT
  CONFIG <- const$CONFIG
  seqM <- seq(1,M)
  filenames <- list()
  ## add leading zeroes to the file names to avoid confusion
  zerosM <- sprintf("%04.0f", seqM) #"%05.0f" if > 10^5 runs, etc.

  traits <- colnames(post.samps)

  samps <- halton(n = M, dim = ncol(post.samps)) 
  colnames(samps) <- traits
  
  ## Insert samples from trait posteriors into config.xml files
  for (m in seqM) {
    zm <- zerosM[m]
    PFTi <- PFT
    tr <- traits
    for (k in tr) {
      samp <- quantile( post.samps[,k], samps[m,k])
      PFTi <- append.xmlNode(PFTi, xmlNode(k, samp))
      ens.samps[['post']] <- 
    }
    CONFIGi <- append.xmlNode(CONFIG, PFTi)
    file <- paste("config.postsamp",zm,".xml",sep="")
    saveXML(CONFIGi, file = file, indent = TRUE, prefix = '<?xml version=\"1.0\"?>\n<!DOCTYPE config SYSTEM \"ed.dtd\">\n')
    filenames[['post.ensemble']][m]<-file
    rm(PFTi)
  

  ## Insert samples from trait priors into config.xml files
    PFTi <- PFT
    for (k in tr) {
      samp <- quantile(prior.samps[,k], samps[m,k])
      PFTi <- append.xmlNode(PFTi, xmlNode(k, samp))
    }
    CONFIGi <- append.xmlNode(CONFIG, PFTi)
    file <- paste("config.priorsamp",zm,".xml",sep="")
    saveXML(CONFIGi, file = file, indent = TRUE, prefix = '<?xml version=\"1.0\"?>\n<!DOCTYPE config SYSTEM \"ed.dtd\">\n')
    filenames[['prior.ensemble']][m]<-file
    
    PFTm <- PFT
    for (.itr in traits) {
      PFTm <- append.xmlNode(PFTm, xmlNode(.itr, post.dtheta.q[.itr, 'mean']))
    }
    CONFIGm <- append.xmlNode(CONFIG, PFTm)
    file <- "config.postmeans.xml"
    saveXML(CONFIGm, file = file, indent = TRUE, prefix = '<?xml version=\"1.0\"?>\n<!DOCTYPE config SYSTEM \"ed.dtd\">\n')
  }
  
  for ( j in seq(traits)){
    notj <- seq(tr)[-j]
    PFTl <- append.xmlNode(PFT, xmlNode(tr[j], post.dtheta.q[tr[j], 'lcl']))
    PFTu <- append.xmlNode(PFT, xmlNode(tr[j], post.dtheta.q[tr[j], 'ucl']))
    for (k in notj) {
      pmean <- post.dtheta.q[tr[k], 'mean']
      PFTl <- append.xmlNode(PFTl, xmlNode(tr[k], pmean))
      PFTu <- append.xmlNode(PFTu, xmlNode(tr[k], pmean))
    }
    CONFIGl <- append.xmlNode(CONFIG, PFTl)
    CONFIGu <- append.xmlNode(CONFIG, PFTu)
    filel <- paste("config.postlcl.", tr[j],".xml", sep="")
    fileu <- paste("config.postucl.", tr[j],".xml", sep="")
    saveXML(CONFIGl, file = filel, indent = TRUE, prefix = '<?xml version=\"1.0\"?>\n<!DOCTYPE config SYSTEM \"ed.dtd\">\n')
    saveXML(CONFIGu, file = fileu, indent = TRUE, prefix = '<?xml version=\"1.0\"?>\n<!DOCTYPE config SYSTEM \"ed.dtd\">\n')
  }
  
  PFTm <- PFT
  for (.itr in traits) {
    PFTm <- append.xmlNode(PFTm, xmlNode(.itr, prior.dtheta.q[.itr, 'mean']))
  }
  CONFIGm <- append.xmlNode(CONFIG, PFTm)
  file <- "config.priormeans.xml"
  saveXML(CONFIGm, file = file, indent = TRUE, prefix = '<?xml version=\"1.0\"?>\n<!DOCTYPE config SYSTEM \"ed.dtd\">\n')
  
  ##following will test +/- 15% CI
  ##make prior<ucl|lcl><trait>
  for ( j in seq(traits)){
    notj <- seq(traits)[-j]
    PFTl <- append.xmlNode(PFT, xmlNode(traits[j], prior.dtheta.q[traits[j], 'lcl']))
    PFTu <- append.xmlNode(PFT, xmlNode(traits[j], prior.dtheta.q[traits[j], 'ucl']))
    for (k in notj) {
      pmean <- prior.dtheta.q[traits[k], 'mean']
      PFTl <- append.xmlNode(PFTl, xmlNode(traits[k], pmean))
      PFTu <- append.xmlNode(PFTu, xmlNode(traits[k], pmean))
    }
    CONFIGl <- append.xmlNode(CONFIG, PFTl)
    CONFIGu <- append.xmlNode(CONFIG, PFTu)
    filel <- paste("config.priorlcl.", traits[j],".xml", sep="")
    fileu <- paste("config.priorucl.", traits[j],".xml", sep="")
    saveXML(CONFIGl, file = filel, indent = TRUE, prefix = '<?xml version=\"1.0\"?>\n<!DOCTYPE config SYSTEM \"ed.dtd\">\n')
    saveXML(CONFIGu, file = fileu, indent = TRUE, prefix = '<?xml version=\"1.0\"?>\n<!DOCTYPE config SYSTEM \"ed.dtd\">\n')
  }
  save(filenames, file = 'filenames.Rdata')
}
