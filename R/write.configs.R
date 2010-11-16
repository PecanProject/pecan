write.configs <- function(M, SA, pft, prior.samps, post.samps, q, outdir) {

  const <- pecan.config.constants(pft)
  PFT <- const$PFT
  CONFIG <- const$CONFIG
  tr <- colnames(post.samps)
  filenames <- list()

  if (M>0) { #if M==0, only do sens.anal.runs
    seqM <- seq(1,M)
    ## add leading zeroes to the file names to avoid confusion
    zerosM <- sprintf("%04.0f", seqM) #"%05.0f" if > 10^5 runs, etc.


    samps <- halton(n = M, dim = ncol(post.samps)) 
    colnames(samps) <- tr
    
    ## Insert samples from trait posteriors into config.xml files
    for (m in seqM) {
      zm <- zerosM[m]
      PFTi <- PFT
      for (k in tr) {
        samp <- quantile( post.samps[,k], samps[m,k])
        PFTi <- append.xmlNode(PFTi, xmlNode(k, samp))

      }
      CONFIGi <- append.xmlNode(CONFIG, PFTi)
      file <- paste(outdir, "/config.postsamp",zm,".xml",sep="")
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
      file <- paste(outdir, "/config.priorsamp",zm,".xml",sep="")
      saveXML(CONFIGi, file = file, indent = TRUE, prefix = '<?xml version=\"1.0\"?>\n<!DOCTYPE config SYSTEM \"ed.dtd\">\n')
      filenames[['prior.ensemble']][m]<-file
      
      PFTm <- PFT
      for (tri in tr) {
        PFTm <- append.xmlNode(PFTm, xmlNode(tri, post.dtheta.q[tri, 'mean']))
      }
      CONFIGm <- append.xmlNode(CONFIG, PFTm)
      file <- paste(outdir, "/config.postmeans.xml", sep = '')
      filenames[['postmeans']] <- file
      saveXML(CONFIGm, file = file, indent = TRUE, prefix = '<?xml version=\"1.0\"?>\n<!DOCTYPE config SYSTEM \"ed.dtd\">\n')
    }
  }

  if (SA) { #if SA is true    
    for ( j in seq(tr)){
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
      filel <- paste(outdir, "/config.postlcl", q*100,".", tr[j],".xml", sep="")
      fileu <- paste(outdir, "/config.postucl", q*100,".", tr[j],".xml", sep="")
      filenames[['postSA']] <- c(filel, fileu)
      saveXML(CONFIGl, file = filel, indent = TRUE, prefix = '<?xml version=\"1.0\"?>\n<!DOCTYPE config SYSTEM \"ed.dtd\">\n')
      saveXML(CONFIGu, file = fileu, indent = TRUE, prefix = '<?xml version=\"1.0\"?>\n<!DOCTYPE config SYSTEM \"ed.dtd\">\n')
    }
    
    PFTm <- PFT
    for (tri in tr) {
      PFTm <- append.xmlNode(PFTm, xmlNode(tri, prior.dtheta.q[tri, 'mean']))
    }
    CONFIGm <- append.xmlNode(CONFIG, PFTm)
    file <- paste(outdir, "/config.priormeans.xml", sep = '')
    filenames[['priormeans']] <- file
    saveXML(CONFIGm, file = file, indent = TRUE, prefix = '<?xml version=\"1.0\"?>\n<!DOCTYPE config SYSTEM \"ed.dtd\">\n')
    
    ##following will test +/- 15% CI
    ##make prior<ucl|lcl><trait>
    for ( j in seq(tr)){
      notj <- seq(tr)[-j]
      PFTl <- append.xmlNode(PFT, xmlNode(tr[j], prior.dtheta.q[tr[j], 'lcl']))
      PFTu <- append.xmlNode(PFT, xmlNode(tr[j], prior.dtheta.q[tr[j], 'ucl']))
      for (k in notj) {
        pmean <- prior.dtheta.q[tr[k], 'mean']
        PFTl <- append.xmlNode(PFTl, xmlNode(tr[k], pmean))
        PFTu <- append.xmlNode(PFTu, xmlNode(tr[k], pmean))
      }
      CONFIGl <- append.xmlNode(CONFIG, PFTl)
      CONFIGu <- append.xmlNode(CONFIG, PFTu)
      filel <- paste(outdir, "/config.priorlcl", q*100,".", tr[j],".xml", sep="")
      fileu <- paste(outdir, "/config.priorucl", q*100,".", tr[j],".xml", sep="")
      filenames[['priorSA']] <- c(filel, fileu)
      saveXML(CONFIGl, file = filel, indent = TRUE, prefix = '<?xml version=\"1.0\"?>\n<!DOCTYPE config SYSTEM \"ed.dtd\">\n')
      saveXML(CONFIGu, file = fileu, indent = TRUE, prefix = '<?xml version=\"1.0\"?>\n<!DOCTYPE config SYSTEM \"ed.dtd\">\n')
    }
  }
  save(filenames, file = 'filenames.Rdata')
}


##M  = integer, if = 0 no ens. runs
##SA = logical if TRUE, run SA
##q  = quantiles sampled in the meta-analysis
