library('PECAn', lib.loc = '~/lib/R')

date <-  as.numeric(system("echo $DATE", intern =TRUE))
pecanhome <- system("echo $PECANHOME", intern=TRUE)
pecanout <- paste(pecanhome, '/out', sep = '')
outdir   <- paste(pecanout, date, sep = '')
load(paste(pecanout, '/pecan.samps.Rdata', sep=''))
outdir   <- paste(pecanout, date, sep = '')
##need to be loaded after pecan.samps.Rdata unless values included there
yr0 <- as.numeric(system("echo $YR0", intern =TRUE))
yrf   <- as.numeric(system("echo $YRF", intern =TRUE))
dtheta.q <- list(prior = prior.dtheta.q, post = post.dtheta.q)
saout <- pecan.SA(M, yr0, yrf, outdir, dtheta.q)
satables <- saout[['satables']]
transformed.samps <- saout[['transformed.samps']] 
print(outdir)
save.image(file=paste(outdir, 'pecan.SAout.Rdata', sep=''))
save(satables, file = paste(outdir,'/satables.Rdata', sep = ''))
save(transformed.samps, file = paste(outdir,'/transformed.samps.Rdata',sep=''))


##Sensitivity Analysis Plots
# plot.sa(satables, runtype, outvar)
#saplots <- list('prior' = list(), 'post'=list())
#for (runtype in c('prior','post')) {
#  for (outvar in c('agb','ssc')) {
    ## pdf(paste(runtype, outvar, 'SAplot.pdf', sep=""), width = 11, height = 7)
    ## arrange(plot1,plot2,plot3,plot3,ncol=4)
    ## grid.arrange(plot0, plot1, plot2, plot3, ncol=4)
#    pdf(paste(runtype,outvar,'SAplot.pdf',sep=""))
#    plot.sa(satables,runtype,outvar)
#    dev.off()
#    #saplots[[runtype]][[outvar]] <- plot.sa(satables,runtype,outvar)
#  }
#}
