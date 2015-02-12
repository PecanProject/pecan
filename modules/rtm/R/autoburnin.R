### Auto burn-in

setwd("/projectnb/dietzelab/ashiklom/pecan/modules/rtm/R")
library(data.table)
library(coda)

autoburnin <- function(path,
                       outpath="../run_results/autoburnin/",
                       gd.threshold=1.1,
                       min.thin=1){
    flist <- list.files(path)
    flist <- flist[grep(".*\\.dat", flist)]
    flist <- flist[which(nchar(flist) > 5)]
    species.list <- unique(gsub("(.*)_[0-9][.][0-9]+_.*", "\\1", flist))
    
    for(species in species.list){
        speciesre <- sprintf("^%s_.*\\.dat", species)
        fset <- flist[grep(speciesre, flist)]
        fset <- fset[which(nchar(fset) > 5)]
        l <- list()
        nf <- system(sprintf("wc -l %s%s_*", path, species), TRUE)
        n <- min(as.numeric(
            sapply(nf,function(x) strsplit(x, "\\.\\.")[[1]][1]))[-17])
        if(n > 1e6){
            i.burnin <- floor(n/2)
        } else if (n > 100) {
            i.burnin <- 1
        } else {
            print("Yep, bad run.")
            next
        }
        for (f in fset){
            lf.name <- sprintf("%s%s", path, f)
            lf.head <- fread(lf.name, nrow=1, header=FALSE)[1,]
            lf.body <- data.frame(fread(lf.name, header=FALSE, skip=i.burnin))
            colnames(lf.body) <- lf.head
            l[[f]] <- lf.body
        }
        nst <- min(sapply(l, nrow))
        nsamp <- nst
        nh <- floor(nsamp/2)
        samples <- seq(nh, nsamp - 1)
        nsamp <- nh
        l <- lapply(1:length(l), function(x) l[[x]][samples,])
        l <- mcmc.list(lapply(l, mcmc))
        while(TRUE){
            gd <- gelman.diag(l, autoburnin=FALSE)
            if (gd$mpsrf > gd.threshold){
                nh <- floor(nsamp/2)
                samples <- seq(nh, nsamp-1)
                nsamp <- nh
                if(nsamp*16/min.thin < 5000) {
                    print("Insufficient samples. Failure to converge")
                    break
                } else {
                    l <- lapply(1:length(l), function(x) l[[x]][samples,])
                    l <- mcmc.list(lapply(l, mcmc))
                }
            } else {
                thin <- floor(16 * nsamp / 5000)
                samples <- seq(1, nsamp, by=thin)
                print(sprintf("Converged! burnin = %d, thin = %d",
                              nst - nsamp,
                              thin))
                l <- lapply(1:length(l), function(x) l[[x]][samples,])
                m.bind <- data.frame(do.call(rbind, l), row.names=NULL)
                m.bind$species <- species
                fname <- sprintf("%s%s_samples.csv", outpath, species)
                write.csv(m.bind, fname)
                break
            }
        }
    }
}

## Load results
#autoburnin("../run_results/FFT_Jan27/")
autoburnin("../run_results/FFT_Jan28_leaf/",
           outpath="../run_results/ab_leafre/",
           gd.threshold = 1.5)