#View all spectra

specpath <- "data/SE_spectra/Reflectance/"
flist <- list.files(specpath, full.names = TRUE)

speclist <- lapply(flist, read.csv, header=TRUE)
refl.list <- lapply(speclist, function(x) x[,3])
refl.mat <- do.call(rbind, refl.list)
refl.dat <- data.frame(refl.mat, row.names = flist)

badspec <- which(refl.dat[,1] > 0.8)
flist[badspec]
flist.split <- strsplit(flist, "_")
unique(sapply(flist.split, "[", 6))