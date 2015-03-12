### Exploratory results using raw samples
library(data.table)
library(ggplot2)
library(gridExtra)
load("~/Documents/Unsynced/pecan/modules/rtm/data/samples.Rdata")



# Aggregation -------------------------------------------------------------

spec.s <- samples[,lapply(.SD, median, na.rm=TRUE),
                  by = spectra, .SDcols = 3:7]

# Basic histograms --------------------------------------------------------

N.h <- ggplot(samples) + aes(x = N) + geom_histogram()
Cab.h <- ggplot(samples) + aes(x = Cab) + geom_histogram()
Cw.h <- ggplot(samples) + aes(x = Cw) + geom_histogram()
Cm.h <- ggplot(samples) + aes(x = Cm) + geom_histogram()

grid.arrange(N.h, Cab.h, Cw.h, Cm.h)

