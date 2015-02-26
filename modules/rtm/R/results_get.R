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

get.results <- function(path, species, burnin=5e4, thin=100){
    print(sprintf("Loading species %s...", species))
    flist <- list.files(path)
    speciesre <- sprintf("^%s_.*\\.dat", species)
    fset <- flist[grep(speciesre, flist)]
    fset <- fset[which(nchar(fset) > 5)]
    if(length(fset) > 0){
        speclist <- lapply(fset, function(x) data.frame(fread(paste(path, x, sep=''), 
                                                              header=TRUE)))
        min.spec <- min(sapply(speclist, nrow))
        if(min.spec < 5000){
            print("Insufficient samples found. Bad run")
            return(NA)
        }
        samples <- seq(burnin, min.spec, by=thin)
        print(sprintf("%d chains x %d samples = %d total",
                      length(fset),
                      length(samples),
                      length(samples)*length(fset)))
        speclist <- lapply(speclist, "[", samples,,drop=FALSE)
        speclist <- mcmc.list(lapply(speclist, mcmc))
        return(speclist)
    }
}
