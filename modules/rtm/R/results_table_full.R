SPECIES.PATH <- "miscellaneous/species_info.csv"
species.info <- read.csv(SPECIES.PATH)

unchain <- function(m, m.name){
        m.bind <- data.frame(do.call(rbind, m), row.names=NULL)
        m.bind$species <- gsub("(.*)\\.[a-z]+\\.l$", "\\1", m.name)
        return(m.bind)
}

results.table <- function(path=FALSE){
        if(path!=FALSE){
                get.results(path)
        }
        results.vec <- ls(".GlobalEnv")
        results.vec <- results.vec[grep("\\.l$", results.vec)]
        results.list <- lapply(results.vec, function(x) unchain(get(x), x))
        results.big <- data.frame(do.call(rbind, results.list), row.names=NULL)
        results.big <- merge(results.big, species.info)
        return(results.big)
}
