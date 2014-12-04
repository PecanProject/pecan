library(reshape2)

specmatrix <- function(Species,
                       wln=400,
                       wlx=2500,
                       path="data/SE_spectra/Reflectance/"
                       ){
        flist <- list.files(path)
        flist.split <- strsplit(flist, "_")
        fset.inds <- which(sapply(flist.split, "[", 5) == Species)
        fset <- flist[fset.inds]
        raw.list <- lapply(fset, function(x) read.csv(paste(path,x,sep=''),
                                                       header=TRUE))
        refl.list <- lapply(raw.list, function(x) {
                x[which(x$Wavelength >= wln &
                                x$Wavelength <= wlx), 3]
        })
        refl.mat <- do.call(cbind, refl.list)
        colnames(refl.mat) <- fset
        return(refl.mat)
}
