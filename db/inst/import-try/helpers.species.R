# Helper functions for discriminating species
library(taxize)
library(data.table)

get.plants.sources <- function(){
    require(taxize)
    common.plants <- c("acer rubrum", "zea mays", "tsuga canadensis")
    dat <- gnr_resolve(common.plants)
    plants.sources <- unique(dat$data_source_title)
    all.sources <- gnr_datasources()
    source.ids <- all.sources[all.sources$title %in% plants.sources, "id"]
    return(source.ids)
}

all.sources <- get.plants.sources()

fix.species <- function(dat){
    require(taxize)
    require(data.table)
    species.old <- tolower(dat[,unique(species_scientific)])
    usda <- 150
    all.sources <- all.sources
    species.matched.usda <- gnr_resolve(names = species.old,
                                        canonical = TRUE,
                                        preferred_data_sources = 150,
                                        best_match_only = TRUE)
    species.matched <- data.table(species.matched.usda)
    species.matched[, submitted_name := tolower(submitted_name)]
    missing.ind <- which(!sapply(species.old, function(x) 
                                 any(agrep(x, species.matched[,submitted_name], 
                                           ignore.case = TRUE))))
    missing.usda <- names(missing.ind)
    if(!all(is.na(missing.usda))){
        print("Missing from USDA:")
        print(missing.usda)
        print("Attempting all sources")
        species.matched.all <- gnr_resolve(names = missing.usda,
                                           canonical = TRUE,
                                           best_match_only = TRUE,
                                           preferred_data_sources = all.sources)
        species.matched.all <- data.table(species.matched.all)
        setkey(species.matched.all, submitted_name)
        species.matched.all <- unique(species.matched.all)
        print(species.matched.all)
        species.matched.all[, submitted_name := tolower(submitted_name)]
        missing.ind2 <- which(!sapply(missing.usda, function(x) 
                                      any(agrep(x, species.matched.all[,submitted_name], 
                                                ignore.case = TRUE))))
        missing.all <- names(missing.ind2)
        if(!all(is.na(missing.all))){
            error.message <- paste("Still couldn't find:", 
                                   paste(missing.all, collapse=", "),
                                   "\n Resolve manually and rerun")
            warning(error.message)
            return(missing.all)
        }
        species.matched <- rbindlist(list(species.matched, species.matched.all))
    } 
    print("All species found!")
    setkey(species.matched, submitted_name)
    dat2 <- copy(dat)
    inds <- sapply(species.matched[,submitted_name],
                   function(x) grep(x, dat2[,species_scientific], ignore.case=TRUE))
    for(sp in names(inds)){
        set(dat2, i=inds[[sp]], j="species_scientific", value=species.matched[sp,matched_name2])
    }
    return(dat2)
}



