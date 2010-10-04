pecan.ens.vals <- function(mcmc.object, priors, n.samples) {

mcmc.mat <- lapply(mcmc.object, as.matrix)
  
samp.n <- dim(mcmc.mat[[1]])[1]

for (pname in traits) {
  if (pname == 'Vcmax') pname <- 'Vm0'
  prior <- priors[pname, c('PriorDistn', 'PriorParamA', 'PriorParamB')]
  colnames (prior) <- c("distn", "a", "b")
  priorsamp <- eval ( parse ( text = paste("r",prior$dist, "(", samp.n , ", ", prior$a, ", ",prior$b, ")",sep = "")))  
  if (pname %in% dbprvec) {
    mcmc.mat[[pname]] <- matrix (priorsamp, nrow = samp.n, ncol = 1)
  }
}

}
