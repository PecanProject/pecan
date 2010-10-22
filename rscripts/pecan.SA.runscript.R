library('PECAn', lib.loc = '~/lib/R')
load('pecan.samps.Rdata')
yr0 <- as.numeric(system("echo $YR0", intern =TRUE))
yrf   <- as.numeric(system("echo $YRF", intern =TRUE))
date <-  as.numeric(system("echo $DATE", intern =TRUE))
user <- system("echo $USER", intern=TRUE)
outdir <- paste('/home/scratch/',user,'/pecan/out',date,sep='')

dtheta.q <- list(prior = prior.dtheta.q, post = post.dtheta.q)
saout <- pecan.SA(M, yr0, yrf, date, outdir,dtheta.q)
satables <- saout[['satables']]
transformed.samps <- saout[['transformed.samps']] 
save(satables, file = paste(outdir,'/satables.Rdata', sep = ''))
save(transformed.samps, file = paste(outdir,'/transformed.samps.Rdata',sep=''))
