source('R/trait.dictionary.R')
source('R/pecan.edout.R')

pecanhome <- system("echo $PWD", intern=TRUE)
pecanout  <- paste(pecanhome, '/out', sep = '')
date      <-  read.table(paste(pecanhome, '/edin/DATE', sep=''))
load(paste(pecanout, '/M.Rdata', sep=''))

outdir    <- paste(pecanout, date, sep = '')
yr0       <- as.numeric(system("echo $YR0", intern =TRUE))
yrf       <- as.numeric(system("echo $YRF", intern =TRUE))
edout     <- pecan.edout(M, yr0, yrf, outdir)

save(edout, file=paste(pecanout, '/edout.Rdata', sep=''))
