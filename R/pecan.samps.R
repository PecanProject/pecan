pecan.samps <- function(trait.mcmc, priors) {

  trait.mat <- lapply(trait.mcmc, as.matrix)
  nodata.traits <- rownames(priors)[!rownames(priors) %in% names(trait.mat)]
  priors$n <- nrow(trait.mat[[1]])

  prior.samps <- sapply(1:nrow(priors), function(x) do.call(pr.samp,as.list(priors[x,])))
  colnames(prior.samps) <- rownames(priors)

  for (ndt in nodata.traits) trait.mat[[ndt]] <- prior.samps[,ndt] 

  ## Convert variables with different units in DB and ED
  ## Convert leaf width in mm to leaf width in m
  trait.mat[['leaf_width']]   <- 1/1000 * trait.mat[['leaf_width']]
  priors['leaf_width', "PriorParamA"] <-  priors['leaf_width', "PriorParamA"] - log(1000)

  ## Transform leafN -> c2n_leaf
  trait.mat[['c2n_leaf']] <- 48/trait.mat[['leafN']][,1]
  trait.mat <- trait.mat[-which(names(trait.mat)=='leafN')]

  c2n_leaf <- do.call(pr.samp, as.list(priors['leafN',]))
  .parms <- signif(fitdistr(c2n_leaf, "lognormal")$estimate, 2)
  priors["c2n_leaf", 1:3] <- c("lnorm", .parms)
  priors <- priors[-which(rownames(priors) == "leafN"),]

  ## root maint resp = 50% of total root resp 
  trait.mat[['root_respiration_factor']] <- 0.5 * trait.mat[['root_respiration_factor']]
  priors['root_respiration_factor', "a"] <- as.numeric(priors['root_respiration_factor', "a"]) - log(2)

  ## only retain mean in mcmc.mat
  for (i in names(mcmc.mat)) mcmc.mat[[i]] <- mcmc.mat[[i]][,1]

  return(list(trait.mat = trait.mat, priors = priors)) 
}
