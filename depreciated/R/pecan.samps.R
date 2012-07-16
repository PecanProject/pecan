#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
pecan.samps <- function(trait.mcmc, priors) {
  trait.mat <- lapply(trait.mcmc, as.matrix)
  trait.nrows <- lapply(trait.mat, function(x) nrow(x))
  n.samp <- do.call(min, trait.nrows)
  traits <- names(trait.mcmc)
  priors$n <- nrow(trait.mat[[1]])
  prior.samps <- as.data.frame(sapply(1:nrow(priors), function(x) do.call(pr.samp,priors[x,])[1:n.samp]))
  colnames(prior.samps) <- rownames(priors)

  post.samps <- prior.samps
  
  for (tri in traits) post.samps[,tri] <- unlist(trait.mcmc[[tri]][, 'beta.o'])[1:n.samp]

  return(list(post = post.samps,
              prior = prior.samps,
              priors = priors)) 
}
