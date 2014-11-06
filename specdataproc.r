library(reshape2)

melt.refdat <- function(datlist){
  mbound <- rbind(datlist)
  wlnames <- colnames(mbound[9:length(mbound)])
  melted <- melt(mbound, measure.vars = wlnames, variable.name = "Reflectance")
  melted$Wavelength <- as.numeric(
    sapply(strsplit(as.character(melted$variable), "_"), "[[", 2)
    )
  melted$variable <- NULL
  
}