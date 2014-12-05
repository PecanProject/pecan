library(coda)

get.results <- function(path, burnin=2000, thin=16){
        flist <- list.files(path)
        flist <- flist[which(nchar(flist) > 5)]
        species.list <- unique(gsub("(.*)_[0-9][.][0-9]+_.*", "\\1", flist))
        
        relist <- c("none", "plot", "leaf")
        
        for (species in species.list){
                for (re in relist){
                        fset <- flist[grep(sprintf("%s_.*_%s_.*", species, re), flist)]
                        print(fset)
                        if(length(fset) > 0){
                                speclist <- lapply(fset, function(x) read.csv(paste(path, x, sep=''), 
                                                                              header=TRUE))
                                min.spec <- min(sapply(speclist, nrow))
                                samples <- seq(burnin, min.spec, by=thin)
                                speclist <- lapply(speclist, "[", samples,,drop=FALSE)
                                speclist <- lapply(speclist, mcmc)
                                assign(sprintf("%s.%s.l", species, re), mcmc.list(speclist), envir = .GlobalEnv)
                        }
                }
        }
}

chain.plots <- function(mcmclist){
        plot(mcmclist)
        t1 <- readline("View details? : ")
        if(t1 == 1){
                l.m <- length(mcmclist)
                out <- NULL
                for (i in 1:l.m){
                        plot(mcmclist[[i]])
                        t2 <- readline("Discard? ")
                        if(t2 == 1){
                                out <- c(out, i)
                        }
                }
                if(length(out) > 0) {
                        return(mcmclist[-out])
                } else {
                        return(mcmclist)
                }
        } else {
                return(mcmclist)
        }
        
}