library('PECAn', lib.loc = '~/lib/R')

date <-  as.numeric(system("echo $DATE", intern =TRUE))
pecanhome <- system("echo $PECANHOME", intern=TRUE)
pecanout <- paste(pecanhome, '/out', sep='')
load(paste(pecanout, '/pecan.samps.Rdata', sep=''))

##need to be loaded after pecan.samps.Rdata unless values included there
yr0 <- as.numeric(system("echo $YR0", intern =TRUE))
yrf   <- as.numeric(system("echo $YRF", intern =TRUE))
dtheta.q <- list(prior = prior.dtheta.q, post = post.dtheta.q)
saout <- pecan.SA(M, yr0, yrf, date, pecanout,dtheta.q)
satables <- saout[['satables']]
transformed.samps <- saout[['transformed.samps']] 
save(satables, file = paste(pecanout,'/satables.Rdata', sep = ''))
save(transformed.samps, file = paste(pecanout,'/transformed.samps.Rdata',sep=''))
