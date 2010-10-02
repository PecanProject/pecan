##  1.7 Number of runs in ensemble
n <- 500
    

  ## 5.7 Write MA model with trait-specific prior
  




.d <- list(c("Vcmax", "Vm0"), c("leafN", "c2n_leaf"))
for (i in 1:2) dbprvec <- gsub(.d[[i]][1], .d[[i]][2], dbprvec)
for (i in 1:2) dbtrvec <- gsub(.d[[i]][1], .d[[i]][2], dbtrvec)

save(mcmc.mat, pft, priors, dbprvec, dbtrvec, log, file='out.dbquery.Rdata')
save(priors, traits, pft, file = 'dbquery.priors.Rdata')


source('plot.priors.R')

pdf (paste(pft, "post.pdf", sep = ""))
