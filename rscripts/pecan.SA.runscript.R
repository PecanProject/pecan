library(PECAn, lib.loc='~/lib/R')
load('out/edout.Rdata')
load('out/pecan.samps.Rdata')
dtheta.q <- list(prior = prior.dtheta.q, post = post.dtheta.q)
saout <- pecan.SA(edout, dtheta.q)
satables <- saout[['satables']]
transformed.samps <- saout[['transformed.samps']] 
save(satables, file='out/satables.Rdata')
save(transformed.samps, file = 'out/transformed.samps.Rdata')


saplots<-list()
for (outvar in c('agb', 'ssc')){
  pdf(paste('saplots',outvar,'.pdf', sep=''))
  plot.sa(satables, outvar)
  dev.off()
}
