library(reshape2)

melt.refdat <- function(datlist){
  mbound <- do.call(rbind, datlist)
  wlnames <- colnames(mbound[9:length(mbound)])
  melted <- melt(mbound, measure.vars = wlnames, value.name = "Value")
  melted$Wavelength <- as.numeric(
    sapply(strsplit(as.character(melted$variable), "_"), "[[", 2)
    )
  melted$variable <- NULL
  melted$Instrument <- substring(melted$Instrument, 1, 1)
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

specmatrix <- function(in.full, 
                       conditions = expression(Species == "Grape" &
                                                 Spectra_Type == "Refl" &
                                                 Wavelength >= 400)){
  in.dat <- subset(in.full, eval(conditions))
  in.dat$ID <- paste(in.dat$Spectra_Name, in.dat$Sample_Date, in.dat$Instrument, sep="__")
  in.dat$ID <- as.integer(factor(in.dat$ID))
  out.cast <- dcast(in.dat[c("ID", "Wavelength", "Value")], 
                    Wavelength ~ ID, value.var="Value")
  out.dat <- data.matrix(out.cast)
  return(out.dat)
}

species.list <- function(in.full, Return=FALSE){
  sl <- with(in.full, as.character(unique(Species)))
  if(Return){
    return(sl)
  } else {
    write(sl, file="scripts/species_list.txt",
          ncolumns=1,
          append=FALSE)
  }
}