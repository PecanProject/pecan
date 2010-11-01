pecan.ma.summary <- function(mcmc.object, pft){
  ## G-R diagnostics to ensure convergence
  pdf(paste('out/ma.summaryplots.',pft, '.pdf', sep = ''))
  for (trait in names(mcmc.object)){
    gd<-gelman.diag(mcmc.object[[trait]])
    mpsrf<-round(gd$mpsrf,digits=4)
    if(mpsrf<1.1){
      note <-  cat ("JAGS model converged for", pft, trait,
                    "\nGD MPSRF = ",mpsrf,"\n", sep=" ")
    } else {
      note <- cat ("JAGS model did not converge for", pft, trait,
                     "\nGD MPSRF = ",mpsrf,"\n", sep=" ")
    }
    maparms <- names(trait.mcmc[['SLA']][1,][1][[1]])
    for (i in maparms) {
      plot(mcmc.object[[trait]][,i], trace = FALSE, density = TRUE,
           main = paste('summary plots of',i ,'for', pft, trait))
      plot(trait.mcmc[[trait]][,i],density = FALSE, xlim =c(1, 50000))
      autocorr.plot(mcmc.object[[trait]][,i][1])
      mtext(text = note, line = 3)
    }
  }
  dev.off()
}
