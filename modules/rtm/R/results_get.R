##' @name get.results
##' @title Load MCMC chains
##' @details {
##' Load all MCMC chains from a given folder, of a given species, into a single MCMC list object.
##' Optionally applies burn-in and thinning.
##' }

##' @param path Folder containing run results (e.g. run_results/testfolder/)
##' @param burnin Burn-in amount; i.e. remove the first 'n' values (default = 0)
##' @param thin Thinning interval; i.e. keep only every 'nth' value (default = 1)
##' @param species Name of species to load. Must exactly match species tag in filename. "All" (default) loads all species.
##' @return Returns 'mcmc.list' object of all loaded chains.
##' @export
##' 
##' @author Alexey Shiklomanov

library(coda)
library(data.table)

get.results <- function(path, burnin=5e4, thin=100, species="All"){
        flist <- list.files(path)
        if(species != "All") flist <- flist[grep(species, flist)]
        flist <- flist[which(nchar(flist) > 5)]
        species.list <- unique(gsub("(.*)_[0-9][.][0-9]+_.*", "\\1", flist))
        
        relist <- c("none", "plot", "leaf")
        
        for (species in species.list){
                for (re in relist){
                        fset <- flist[grep(sprintf("%s_.*_%s_.*", species, re), flist)]
                        print(fset)
                        if(length(fset) > 0){
                                speclist <- lapply(fset, function(x) data.frame(fread(paste(path, x, sep=''), 
                                                                              header=TRUE)))
                                min.spec <- min(sapply(speclist, nrow))
                                samples <- seq(burnin, min.spec, by=thin)
                                print(sprintf("%d samples", length(samples)*length(fset)))
                                speclist <- lapply(speclist, "[", samples,,drop=FALSE)
                                speclist <- lapply(speclist, mcmc)
                                assign(sprintf("%s.%s.l", species, re), mcmc.list(speclist), envir = .GlobalEnv)
                        }
                }
        }
}
