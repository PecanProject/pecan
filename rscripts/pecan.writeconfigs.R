library(PECAn, lib.loc = '~/lib/R')
load('out20110119/pecan.MA.Rdata')

pftName <- settings$pft
quantiles <- settings$quantiles
samps <- pecan.samps(trait.mcmc, priors)
save(samps, file='out/pecan.samps.Rdata')



## sample values for ensemble
trait.beta.o <- list()
for(i in names(trait.mcmc)){
  trait.beta.o[[i]] <-   as.matrix(trait.mcmc[[i]][,'beta.o'])
}
trait.posteriors <- as.data.frame(trait.beta.o)
colnames(trait.posteriors) <- names(trait.beta.o)

priors$n <- nrow(trait.posteriors)
colnames(priors)[which(colnames(priors) %in% c('parama','paramb'))] <- c('a', 'b')

samps <- list(prior = sapply(1:nrow(priors), function(x) do.call(pr.samp,priors[x,])),
              post  = sapply(1:nrow(priors), function(x) do.call(pr.samp,priors[x,])))

for (i in names(samps)){
  colnames(samps[[i]]) <- rownames(priors)
}

#???  priors$distn[priors$distn=='weib'] <- 'weibull'

for (tri in ncol(trait.posteriors)) samps[['post']][,tri] <- trait.posteriors[, tri]

save(samps, file='out/pecan.samps.Rdata')


quantiles<-vector()
if (!is.null(settings$quantiles$sigma)){
  sigma <- as.numeric(settings$quantiles[names(settings$quantiles)=='sigma'])
  quantiles <- pnorm(1-sigma)
}
if (!is.null(settings$quantiles$quantile)) {
  quantiles <- append(quantiles, as.numeric(settings$quantiles[names(settings$quantiles)=='quantile']))
}
if (length(quantiles) == 0) {
  quantiles <- 1-pnorm(-3:3) #default
}
if (!0.5 %in% quantiles) {
  quantiles <- append(quantiles, 0.5)
}
quantiles <- sort(quantiles)

calculate.quantiles <- function(x,samps, quantiles) {
  quantile(samps[,x], quantiles)
}


quantile.samples <- list(post  = lapply(traits, calculate.quantiles, samps[['post']], quantiles),
                         prior = lapply(traits, calculate.quantiles, samps[['prior']], quantiles))


for(i in names(quantile.samples)) {
  names(quantile.samples[[i]]) <- traits
}

save(quantile.samples, file = "out/quantile.samples.Rdata")

## generate config files

write.configs(ensemble_size, sensitivity_analysis, pft, ens.samps, quantile.samples, outdir, quantiles)

save(samps, file="samps.Rdata')

