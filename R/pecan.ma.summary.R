pecan.ma.summary <- function(mcmc.object, pft){
  ## G-R diagnostics to ensure convergence
  pdf(paste(pft, 'priors.pdf', sep = ''))
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

    par(mfrow = c(3,2))
    autocorr.plot(mcmc.object[[trait]][,1][1])
    plot(mcmc.object[[trait]][,1], trace = FALSE, density = TRUE,
         main = paste('posterior pdf of mu for', pft, trait))
    mtext(text = note, line = 3)
  } 
  dev.off()
}
