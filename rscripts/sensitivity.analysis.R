library(PECAn, lib.loc='~/lib/R')

load('settings.Rdata')
load(paste(outdirs[i], 'sample.ensemble.RData', sep=''))
load(paste(outdir, 'trait.samples.Rdata')
load(paste(outdir, "sa.samples.Rdata", sep=''))


dtheta.q <- list(prior = prior.dtheta.q, post = post.dtheta.q)
saout <- pecan.SA(edout, dtheta.q)
satables <- saout[['satables']]
transformed.samps <- saout[['transformed.samps']] 
save(satables, file='out/satables.Rdata')
save(transformed.samps, file = 'out/transformed.samps.Rdata')

for (outvar in c('agb')){
  plot.sa(satables, outvar)
}
