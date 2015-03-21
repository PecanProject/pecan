### Generate input plots for a given species

source("input_matrix.R")
library(ggplot2)
library(reshape2)

specieslist <- as.character(read.csv("../prospect_bayes/scripts/species_list.txt", header=FALSE)[,1])

input.plot <- function(Species, nsd=3, PATH="~/Documents/Dropbox/SE_spectra/Reflectance/"){
        mat <- specmatrix(Species, path=PATH)
        df1 <- data.frame(mat)
        df1$Wavelength <- 400:2500
        df1.melt <- melt(df1, value.name="Reflectance", variable.name="Spectrum", id.vars="Wavelength")
        df1.melt$Leaf <- gsub("(.*)_[0-9]{5}\\.csv", "\\1", df1.melt$Spectrum)
        df1.melt$Leaf <- factor(sprintf("%02d", as.integer(as.factor(df1.melt$Leaf))))
        
        plt <- ggplot(df1.melt) +
                geom_line(aes(x=Wavelength,
                              y=Reflectance,
                              color=Leaf),
                          alpha=0.6) +
                ggtitle(Species)
       plot(plt)
}
