library(coda)

flist <- list.files("run_results/")
flist <- flist[which(nchar(flist) > 5)]
species.list <- unique(gsub("(.*)_[0-9][.][0-9]+_.*", "\\1", flist))

relist <- c("none", "plot", "leaf")


for (species in species.list){
  for (re in relist){
    fset <- flist[grep(sprintf("%s_.*_%s_.*", species, re), flist)]
    print(fset)
    speclist <- lapply(fset, function(x) read.csv(paste("run_results/", x, sep=''), 
                                                  header=TRUE))
    min.spec <- min(sapply(speclist, nrow))
    speclist <- lapply(speclist, "[", 1:min.spec,,drop=FALSE)
    speclist <- lapply(speclist, mcmc)
    assign(sprintf("%s.%s.l", species, re), mcmc.list(speclist))
  }
}