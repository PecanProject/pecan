library(coda)

path <- "~/Dropbox/run_results/2014_12_2/"
flist <- list.files(path)
flist <- flist[which(nchar(flist) > 5)]
species.list <- unique(gsub("(.*)_[0-9][.][0-9]+_.*", "\\1", flist))

relist <- c("none", "plot", "leaf")

burnin <- 2000

for (species in species.list){
  for (re in relist){
    fset <- flist[grep(sprintf("%s_.*_%s_.*", species, re), flist)]
    print(fset)
    speclist <- lapply(fset, function(x) read.csv(paste(path, x, sep=''), 
                                                  header=TRUE))
    min.spec <- min(sapply(speclist, nrow))
    speclist <- lapply(speclist, "[", burnin:min.spec,,drop=FALSE)
    speclist <- lapply(speclist, mcmc)
    assign(sprintf("%s.%s.l", species, re), mcmc.list(speclist))
  }
}