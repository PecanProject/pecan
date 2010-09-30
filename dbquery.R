##  1.7 Number of runs in ensemble
n <- 500
    
  ## 5.4 Clean up data
  data <- data[,c(3, 5, 6, 7, 8)]                    # drop trait stat name column
  colnames(data) <- c("Y", "se", "n", "site","trt")  # rename cols
  data$stdev <- NA                                   # create col for sd
  data$obs.prec <- NA                                # create col for obs.prec
  
  ## 5.5 Calculate obs.prec
  ##  for leaf%N, transform mean and sd, assume leaf is 48%C
  j <- 1
  data$stdev <- sqrt(data$n) * data$se # sd for all other traits
  data$obs.prec <- 1 / sqrt(data$stdev)
  
  prior <- priors[pname, c('PriorDistn', 'PriorParamA', 'PriorParamB', 'PriorN')]
  colnames(prior) <- c("distn", "a", "b", "n")
  ## 5.7 Write MA model with trait-specific prior
  jag.model.file <-  paste( name, ".model.bug",sep="")  # file to store model
  write.ma.model ( ma.model, jag.model.file,
                    prior$distn, prior$a, prior$b,
                    length ( data$Y ), length(unique(data$trt)), length(unique(data$site)))

  j.data     <- data[,c(1,3,4,5,7)] #Y,n,site,trt, obs.prec
  j.data$n[is.na(j.data$n)] <- 1
  j.model    <- jags.model ( file = jag.model.file,
                            data = j.data,
                            n.adapt = j.adapt,
                            n.chains = j.chains)
  mcmc.object <- coda.samples ( model = j.model,
                               variable.names = j.vars,
                               n.iter = j.iter,
                               thin = j.thin)
  
  ## 5.8 G-R diagnostics to ensure convergence
  gd<-gelman.diag(mcmc.object)
  mpsrf<-round(gd$mpsrf,digits=4)
  if(mpsrf<1.1){
    note <-  paste ("JAGS model converged for", pft, pname,
                    "GD MPSRF = ",mpsrf, sep=" ")
  } else {
    note <- paste ("JAGS model did not converge for", pft, pname,
                   "GD MPSRF = ",mpsrf, sep=" ")
  }
  log[['gelman_diagnostic']][name] <- note
  
  ## 5.9 make simple diagnostic plots
  plot(mcmc.object[,1], trace = FALSE, density = TRUE, main = paste('posterior pdf of beta.o for', pft, name))
  mtext(text = note, line = 3)
  

  ## 5.10 data storage

  mcmc.mat[[pname]] <- as.matrix(mcmc.object)
}
dev.off()

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


## 5.11 Convert variables with different units in DB and ED
## 5.11.1 convert leaf width in mm to leaf width in m
mcmc.mat[['leaf_width']]   <- 1/1000 * mcmc.mat[['leaf_width']]
priors['leaf_width', "PriorParamA"] <-  priors['leaf_width', "PriorParamA"] - log(1000)

## 5.11.2 leafN --> c2n_leaf
mcmc.mat[['c2n_leaf']] <- 48/mcmc.mat[['leafN']]
mcmc.mat <- mcmc.mat[-which(names(mcmc.mat)=='leafN')]

.d <- priors["leafN", c("PriorDistn", "PriorParamA", "PriorParamB")]
priors <- priors[-which(rownames(priors) == "leafN"),]
c2n_leaf <- eval (parse (text = paste("48/r", .d[1],"(100000", ", ", .d[2], ", ", .d[3], ")", sep = "")))
.parms <- signif(fitdistr(c2n_leaf, "lognormal")$estimate, 2)
priors["c2n_leaf", c("PriorDistn", "PriorParamA", "PriorParamB")] <- c("lnorm", .parms)

## 5.11.3 root maint resp = 50% of total root resp 
mcmc.mat[['root_respiration_factor']] <- 0.5 * mcmc.mat[['root_respiration_factor']]
priors['root_respiration_factor', "PriorParamA"] <- as.numeric(priors['root_respiration_factor', "PriorParamA"]) - log(2)


.d <- list(c("Vcmax", "Vm0"), c("leafN", "c2n_leaf"))
for (i in 1:2) dbprvec <- gsub(.d[[i]][1], .d[[i]][2], dbprvec)
for (i in 1:2) dbtrvec <- gsub(.d[[i]][1], .d[[i]][2], dbtrvec)

save(mcmc.mat, pft, priors, dbprvec, dbtrvec, log, file='out.dbquery.Rdata')
save(priors, traits, pft, file = 'dbquery.priors.Rdata')


source('plot.priors.R')

pdf (paste(pft, "post.pdf", sep = ""))
