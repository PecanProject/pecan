### Generate input plots for a given species

library(ggplot2, reshape2)
source("specdataproc.R")

input.plot <- function(Species, PATH="~/Documents/Dropbox/SE_spectra/Reflectance/"){
        mat <- specmatrix(Species, path=PATH)
        df1 <- data.frame(mat)
        df1$Wavelength <- 400:2500
        df1.melt <- melt(df1, value.name="Reflectance", variable.name="Spectrum", id.vars="Wavelength")
        df1.melt$Leaf <- gsub("(.*)_[0-9]{5}\\.csv", "\\1", df1.melt$Spectrum)
        df1.melt$Leaf <- sprintf("%02d", as.integer(as.factor(df1.melt$Leaf)))
        plt <- ggplot(df1.melt) + aes(x=Wavelength, y=Reflectance, color=Leaf) + geom_line()
        plot(plt)
}
