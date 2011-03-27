if(!is.null(settings$Rlib)){ .libPaths(settings$Rlib)} 
library(PECAn)

load('settings.Rdata')
load(paste(outdirs[i], 'sample.ensemble.RData', sep=''))
load(paste(outdir, 'trait.samples.Rdata'))
load(paste(outdir, "sa.samples.Rdata", sep=''))

trait.defs <- trait.dictionary(traits)

edout.filenames <<- dir(outdir, full.names = TRUE)


dtheta.q <- list(prior = prior.dtheta.q, post = post.dtheta.q)
saout <- pecan.SA(edout, dtheta.q)
satables <- saout[['satables']]
transformed.samps <- saout[['transformed.samps']] 
save(satables, file='out/satables.Rdata')
save(transformed.samps, file = 'out/transformed.samps.Rdata')

for (outvar in c('agb')){
  plot.sa(satables, outvar)
}
