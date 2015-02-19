### Load new samples
library(reshape2)
library(ggplot(violin.plot(2)

PATH.samples <- "~/Documents/Dropbox/run_results/autoburnin/"
PATH.FFTdata <- "~/Documents/Unsynced/pecan/modules/rtm/data/FFT_species_info.csv"
flist <- list.files(PATH.samples, full.names = TRUE)
biglist <- lapply(flist, read.csv, header=TRUE)
results.full <- do.call(rbind, biglist)
results.full$X <- NULL

FFT.info <- read.csv(PATH.FFTdata, header=TRUE)
FFT.info$X <- NULL

results <- merge(results.full, FFT.info, by.x="species", by.y="Label")
colnames(results)[1] <- "Symbol"

violin.plot(violin.plot( <- function(yaxis, xaxis, color){
        plt <- ggplot(violin.plot((results) + aes_string(x=xaxis, y=yaxis, fill=color) +
                geom_violin()
        return(plt)
}

plot(violin.plot("N", "Symbol", "Family"))
plot(violin.plot("Cab", "Symbol", "Family"))
plot(violin.plot("Cw", "Symbol", "Family"))
plot(violin.plot("Cm", "Symbol", "Family"))
plot(violin.plot("sd", "Symbol", "Family"))

plot(violin.plot("N", "Family", "Family"))
plot(violin.plot("Cab", "Family", "Family"))
plot(violin.plot("Cw", "Family", "Family"))
plot(violin.plot("Cm", "Family", "Family"))
plot(violin.plot("sd", "Family", "Family"))

results.dt <- data.table(results)
species.means <- results.dt[,lapply(.SD, mean), by=Symbol, .SDcols=2:6]
species.means <- results.dt[,lapply(.SD, quantile, 0.025), by=Symbol, .SDcols=2:6]
species.means <- results.dt[,lapply(.SD, quantile, 0.975), by=Symbol, .SDcols=2:6]

family.means <- results.dt[,lapply(.SD, mean), by=Family, .SDcols=2:6]
family.means <- results.dt[,lapply(.SD, quantile, 0.025), by=Family, .SDcols=2:6]
family.means <- results.dt[,lapply(.SD, quantile, 0.975), by=Family, .SDcols=2:6]
