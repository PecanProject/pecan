pecan.ma.summary <- function(mcmc.object){
  ## G-R diagnostics to ensure convergence
  gd<-gelman.diag(mcmc.object)
  mpsrf<-round(gd$mpsrf,digits=4)
  if(mpsrf<1.1){
    note <-  paste ("JAGS model converged for", pft, pname,
                    "GD MPSRF = ",mpsrf, sep=" ")
  } else {
    note <- paste ("JAGS model did not converge for", pft, pname,
                   "GD MPSRF = ",mpsrf, sep=" ")
  }

  ## make simple diagnostic plots
  plot(mcmc.object[,1], trace = FALSE, density = TRUE, main = paste('posterior pdf of beta.o for', pft, name))
  mtext(text = note, line = 3)
  
