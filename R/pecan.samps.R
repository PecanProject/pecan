pecan.samps <- function(trait.mcmc, priors) {

  trait.mat <- lapply(trait.mcmc, as.matrix)
  trait.nrows <- lapply(trait.mat, function(x) nrow(x))
  n.samp <- do.call(min, trait.nrows)
  traits <- names(trait.mcmc)
  priors$n <- nrow(trait.mat[[1]])
  colnames(priors)[which(colnames(priors) %in% c('parama','paramb'))] <- c('a', 'b')

  prior.samps <- sapply(1:nrow(priors), function(x) do.call(pr.samp,priors[x,]))
  colnames(prior.samps) <- rownames(priors)

  post.samps <- prior.samps
  for (tri in traits) post.samps[1:n.samp,tri] <- trait.mat[[tri]][1:n.samp, 'beta.o']

  return(list(post.samps = post.samps,
              prior.samps = prior.samps,
              priors = priors)) 
}
