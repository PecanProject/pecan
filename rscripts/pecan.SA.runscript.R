library('PECAn', lib.loc = '~/lib/R')
load('pecan.samps.Rdata')
yr0 <- as.numeric(system("echo $YR0", intern =TRUE))
yrf   <- as.numeric(system("echo $YRF", intern =TRUE))
date <- read.table("DATE")
user <- system("echo $USER", intern=TRUE)
outdir <- paste('/home/scratch/',user,'/pecan/out',date,sep='')
saout <- pecan.SA(M, yr0, yrf, date, outdir)
satables <- saout[['satables']]
transformed.samps <- saout[['transformed.samps']] 
save(satables, file = paste(outdir,'/satables.Rdata', sep = ''))
save(transformed.samps, file = paste(outdir,'/transformed.samps.Rdata',sep=''))
