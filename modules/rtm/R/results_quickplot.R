n <- read.table("FFTspecies_plots.txt")[,1]
for(s in n){
    try({
        l <- get.results(path.results, s, 1, 1)
        pdf(sprintf("gelman_plots/%s.pdf", i))
        gelman.plot(l, ylim=c(1,1.5), autoburnin=FALSE)
        dev.off()
        l.b <- mcmc.list(lapply(1:length(l), function(x) mcmc(l[[x]][-5e3:0,])))
        pdf(sprintf("traceplots/%s.pdf", i))
        plot(l.b)
        dev.off()
    })
}