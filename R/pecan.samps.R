pecan.samps <- function(trait.mcmc, priors) {

  trait.mat <- lapply(trait.mcmc, as.matrix)
  traits <- names(trait.mcmc)
  priors$n <- nrow(trait.mat[[1]])
  colnames(priors)[which(colnames(priors) %in% c('parama','paramb'))] <- c('a', 'b')

  prior.samps <- sapply(1:nrow(priors), function(x) do.call(pr.samp,priors[x,]))
  colnames(prior.samps) <- rownames(priors)

  post.samps <- prior.samps
  for (tri in traits) post.samps[,dt] <- trait.mat[[tri]][,1]

  ## Convert variables with different units in DB and ED
  ## Convert leaf width in mm to leaf width in m
  post.samps[,'leaf_width'] <- post.samps[,'leaf_width']/1000
  prior.samps[,'leaf_width'] <- prior.samps[,'leaf_width']/1000
  priors['leaf_width', "a"] <-  priors['leaf_width', "a"] - log(1000)

  ## Transform leafN -> c2n_leaf
  post.samps[,'leafN'] <- 48/post.samps[,'leafN']
  prior.samps[,'leafN'] <- 48/prior.samps[,'leafN']
  colnames(post.samps)[which(colnames(post.samps) == 'leafN')] <- 'c2n_leaf'
  colnames(prior.samps)[which(colnames(prior.samps) == 'leafN')] <- 'c2n_leaf'

  c2n_leaf <- do.call(pr.samp, as.list(c(priors['leafN',1:3],100000)))
  .parms <- signif(fitdistr(c2n_leaf, "lognormal")$estimate, 2)
  priors["c2n_leaf", 1:3] <- c("lnorm", .parms)
  priors <- priors[-which(rownames(priors) == "leafN"),]

  ## root maint resp = 50% of total root resp 
  prior.samps[,'root_respiration_factor'] <- 0.5 * prior.samps[,'root_respiration_factor']
  post.samps[,'root_respiration_factor'] <- 0.5 * post.samps[,'root_respiration_factor']
  priors['root_respiration_factor', "a"] <- as.numeric(priors['root_respiration_factor', "a"]) - log(2)

  return(list(post.samps = post.samps,
              prior.samps = prior.samps,
              priors = priors)) 
}
