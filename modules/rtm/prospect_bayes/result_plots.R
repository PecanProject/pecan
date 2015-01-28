##' Results plots
##' 
##' Generates bar plots with error bars. 

library(reshape2, ggplot2, GGally)
source("process_output.R")
setwd("../Research/pecan/modules/rtm/prospect_bayes/")
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
species.short <- species.info[,c(1,2)]
results <- merge(results, species.info, by = "species")

bigtable <- results.big()
bigtable.melt <- melt(bigtable, value.name="value", id.vars=c("species", "abbr"))

bigtext <- theme(axis.text.x=element_text(size=14),
                 axis.text.y=element_text(size=14),
                 strip.text.y=element_text(size=20))

N.mean <- with(subset(bigtable.melt, variable=="N"), tapply(value, abbr, mean))
sort.by.N <- names(sort(N.mean))
results.melt$abbr <- factor(results.melt$abbr, sort.by.N)
results$abbr <- factor(results$abbr, sort.by.N)
bigtable$abbr <- factor(bigtable$abbr, sort.by.N)

## Pairs plot
ggpairs(bigtable, 2:5, color="abbr", params=list(corSize=12))


### Violin plot
violin.plt <- function(var, tab){
        plt <- ggplot(subset(tab, variable==var)) +
                aes(x=species, y=value, fill=species) +
                geom_violin() +
                guides(fill=FALSE)
        return(plt)
}

### Bar plot
bar.plt <- ggplot(results) + 
        aes(y=mean, x=abbr, ymin=ci_low, ymax=ci_up, width=0.5) + 
        geom_bar(stat="identity", fill="dark green") +
        geom_errorbar() +
        facet_grid(variable ~ ., scales = "free_y")

### Pointrange
pointrange.plt <- ggplot(results) + 
        aes(y=mean, x=abbr, ymin=ci_low, ymax=ci_up) + 
        geom_pointrange() +
        facet_grid(variable ~ ., scales = "free_y")




### Credible interval plots

## Generate credible intervals
results.vec <- ls()
results.vec <- results.vec[grep("\\.l$", results.vec)]

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
        out.frame$species <- species
        return(out.frame)
}

# for (nm in results.vec) credible.interval(nm)

credints.list <- lapply(results.vec, credible.interval)
credints <- do.call(rbind, credints.list)
credints <- merge(credints, species.short, by="species")
credints$abbr <- factor(credints$abbr, sort.by.N)


ci.text.gg <- theme(axis.text=element_text(size=20),
                    axis.title=element_text(size=25))
ggplot(subset(credints, Quantile!="CI Width")) + 
        aes(x=Wavelength, y=Reflectance, color=abbr, group=abbr) + 
        geom_line() + 
#        ggtitle("Bayesian credible interval for inversion results") +
        ci.text.gg +
        theme(legend.position="none")
ggplot(subset(credints, Quantile=="CI Width")) +
        aes(x=Wavelength, y=Reflectance, color=abbr, group=abbr) +
        geom_line() +
#        ggtitle("Width of Bayesian credible interval for inversion results") +
#        ylab("Reflectance CI width") +
#        xlab("Wavelength") + 
        ci.text.gg +
        theme(legend.position="none")

# predint.list <- lapply(samples.list, 
#                        function(l) apply(l, 1, 
#                                          function(x) rnorm(2101, 
#                                                            prospect4(x[1], x[2], x[3], x[4], n.a, cab.a, w.a, m.a),
#                                                            x[5])))
# predints <- lapply(predint.list, function(l) apply(l, 1, quantile, c(0.025, 0.975)))
                                         
