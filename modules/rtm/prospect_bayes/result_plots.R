##' Bar plots
##' 
##' Generates bar plots with error bars. 

library(reshape2)
library(ggplot2)
source("process_output.R")
source("prospect.R")

RESULTS.PATH <- "~/Documents/Dropbox/run_results/1206-none-1/"
SPECIES.PATH <- "miscellaneous/species_info.csv"

### Load results
get.results(RESULTS.PATH)
rm(IC.none.l, LC.none.l, PICO.none.l, PILA.none.l, PIPO.none.l)

### Generate resutls table with confidence intervals for bar plots
results.raw <- results.table()
results.melt <- melt(results.raw, id.vars=c("species", "vartype"))
results.melt$species <- gsub("(.*)\\.[noneleaf]{4}\\.l", "\\1", results.melt$species)
results <- dcast(results.melt, species + variable ~ vartype, value.var="value")
species.info <- read.csv(SPECIES.PATH)
results <- merge(results, species.info, by = "species")

### Bar plot
ggplot(results) + 
        aes(y=mean, x=abbr, ymin=ci_low, ymax=ci_up, width=0.5) + 
        geom_bar(stat="identity", fill="dark green") +
        geom_errorbar() +
        facet_grid(variable ~ ., scales = "free_y")

### Pointrange
ggplot(results) + 
        aes(y=mean, x=abbr, ymin=ci_low, ymax=ci_up) + 
        geom_pointrange() +
        facet_grid(variable ~ ., scales = "free_y")

### Bar plot of CI width
ggplot(results) + 
        aes(y=ci_range, x=abbr, width=0.5) + 
        geom_bar(stat="identity", fill="dark green") +
        facet_grid(variable ~ ., scales = "free_y")

### Bar plots of results (alternate - pointrange)
plotbar <- function(pvar) {
        ggplot(subset(results, variable==pvar)) +
        aes(x=abbr, y=mean) +
#         aes(x=reorder(abbr, C3, sort), y=mean, color=C3) +
#         geom_pointrange(aes(ymin=ci_low, ymax=ci_up)) +
        geom_bar(stat = "identity") +
        geom_errorbar(aes(ymin=ci_low, ymax=ci_up)) +
        ylab(pvar) + xlab("Species")
}


### Credible interval plots

## Generate credible intervals
results.vec <- ls()
results.vec <- results.vec[grep("\\.l", results.vec)]

credible.interval <- function(nm){
        result <- do.call(rbind, get(nm))
        samples <- result[sample(1:dim(result)[1], 500, FALSE),]
        samp.outs <- apply(samples, 1, function(x) prospect4(x[1], x[2], x[3], x[4], n.a, cab.a, w.a, m.a))
        credint <- apply(samp.outs, 1, quantile, c(0.025, 0.5, 0.975))
        credint <- rbind(credint, credint[3,] - credint[1,])
        rownames(credint)[4] <- "CI Width"
        species <- gsub("(.*)\\.[noneleaf]{4}\\.l", "\\1", nm)
        out.frame <- melt(t(credint))
        colnames(out.frame) <- c("Wavelength", "Quantile", "Reflectance")
        out.frame$Wavelength <- out.frame$Wavelength + 399
        out.frame$Species <- species
        return(out.frame)
}

# for (nm in results.vec) credible.interval(nm)

credints.list <- lapply(results.vec, credible.interval)
credints <- do.call(rbind, credints.list)
ggplot(subset(credints, Quantile!="CI Width")) + 
        aes(x=Wavelength, y=Reflectance, color=Species, group=Species) + 
        geom_line() + 
        ggtitle("Bayesian credible interval for inversion results")
ggplot(subset(credints, Quantile=="CI Width")) +
        aes(x=Wavelength, y=Reflectance, color=Species, group=Species) +
        geom_line() +
        ggtitle("Width of Bayesian credible interval for inversion results") +
        ylab("Reflectance CI width") +
        xlab("Wavelength")
# predint.list <- lapply(samples.list, 
#                        function(l) apply(l, 1, 
#                                          function(x) rnorm(2101, 
#                                                            prospect4(x[1], x[2], x[3], x[4], n.a, cab.a, w.a, m.a),
#                                                            x[5])))
# predints <- lapply(predint.list, function(l) apply(l, 1, quantile, c(0.025, 0.975)))
                                         