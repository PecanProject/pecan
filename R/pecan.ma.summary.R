pecan.ma.summary <- function(mcmc.object, pft){
  for (trait in names(mcmc.object)){
    ## G-R diagnostics to ensure convergence    
    gd<-gelman.diag(mcmc.object[[trait]])
    mpsrf<-round(gd$mpsrf,digits=4)
    if(mpsrf<1.1){
      note <-  cat ("JAGS model converged for", pft, trait,
                    "\nGD MPSRF = ",mpsrf,"\n", sep=" ")
    } else {
      note <- cat ("JAGS model did not converge for", pft, trait,
                   "\nGD MPSRF = ",mpsrf,"\n", sep=" ")
    }
    mtext(text = note, line = 3)

    ## reordering maparms so that beta.o etc not sent to end
    .maparms <- names(trait.mcmc[['SLA']][1,][1][[1]])
    .parms <- c('beta.o', 'thetaSD', 'trtSD', 'ySD')
    maparms <- .maparms[ c(which(.maparms %in% .parms), which(!.maparms %in% .parms))]

    ## plots for mcmc diagnosis
    pdf(paste('out/ma.summaryplots.',pft, trait, '.pdf', sep = ''))
    for (i in maparms) {
      plot(mcmc.object[[trait]][,i], trace = FALSE, density = TRUE,
           main = paste('summary plots of',i ,'for', pft, trait))
      plot(trait.mcmc[[trait]][,i],density = FALSE, xlim =c(1, 50000),
           main = note)
      autocorr.plot(mcmc.object[[trait]][,i][1])
    }
    dev.off()
  }
}
