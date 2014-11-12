library(reshape2)

melt.refdat <- function(datlist){
  mbound <- do.call(rbind, datlist)
  wlnames <- colnames(mbound[9:length(mbound)])
  melted <- melt(mbound, measure.vars = wlnames, value.name = "Value")
  melted$Wavelength <- as.numeric(
    sapply(strsplit(as.character(melted$variable), "_"), "[[", 2)
    )
  melted$variable <- NULL
  ### TODO: Pick out details in Spectra_name
  return(melted)
}

load.all.spec <- function(path="data/NASA_HyspIRI_CVARS_Spectra/"){
  flist <- list.files(path)
  flist.p <- sapply(flist, function(x) sprintf("%s%s", path, x))
  dlist <- lapply(flist.p, read.csv, header=TRUE)
  d.f <- melt.refdat(dlist)
  return(d.f)
}

