pecan.ma.summary <- function(mcmc.object, pft,outdir){
  for (trait in names(mcmc.object)){
    ## G-R diagnostics to ensure convergence    
    gd<-gelman.diag(mcmc.object[[trait]])
    mpsrf<-round(gd$mpsrf,digits=4)
    if(mpsrf < 1.1){
      writeLines(paste ("JAGS model converged for", pft, trait,
                    "\nGD MPSRF = ",mpsrf,"\n", sep=" "))
    } else {
      stop( paste ("JAGS model did not converge for", pft, trait,
                   "\nGD MPSRF = ",mpsrf,"\n", sep=" ") )
    }


    ## reordering maparms so that beta.o etc not sent to end
    .maparms <- names(mcmc.object[[trait]][1,][1][[1]])
    .parms <- c('beta.o', 'thetaSD', 'trtSD', 'ySD')
    maparms <- .maparms[ c(which(.maparms %in% .parms), which(!.maparms %in% .parms))]

    ## plots for mcmc diagnosis
    pdf(paste(outdir,'ma.summaryplots.',pft, trait, '.pdf', sep = ''))
    for (i in maparms) {
      plot(mcmc.object[[trait]][,i], trace = FALSE, density = TRUE,
           main = paste('summary plots of',i ,'for', pft, trait))
      plot(mcmc.object[[trait]][,i],density = FALSE)
      autocorr.plot(mcmc.object[[trait]][,i][1], xlim = c(1, 50))
    }
    dev.off()
  }
}
