##' Functions for converting spectral data into matrices for use in Bayesian inversion.

library(reshape2)

##' @name specmatrix
##' @title Observed reflectance matrix
##' @details {
##' Binds requisite spectra from '.csv' files into a matrix useable by Bayesian inversion function.
##' }
##' 
##' @param Species Name of species to be used. Must EXACTLY match species tag in file name.
##' @param wln Starting wavelength (default=400)
##' @param wlx Maximum wavelength (default=2500)
##' @param path Path to '.csv' spectra (default="data/SE_spectra/Reflectance/")
##' @return Observed reflectance matrix usable by Bayesian inversion function.
##' @export
##' @author Alexey Shiklomanov

specmatrix <- function(Species,
                       wln=400,
                       wlx=2500,
                       path="~/Documents/Dropbox/SE_spectra/Reflectance/"
                       ){
        print(sprintf("Loading %s ...", Species))
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
        RowMeans <- rowMeans(refl.mat)
        RowSD <- apply(refl.mat, 1, sd)
        MaxFilter <- RowMeans + 3 * RowSD
        MinFilter <- RowMeans - 3 * RowSD
        specfilter <- function(spec) any(spec < MinFilter | spec > MaxFilter)
        badspec <- apply(refl.mat, 2, specfilter)
        refl.mat.keep <- refl.mat[,!badspec]
        print(dim(refl.mat) - dim(refl.mat.keep))
        print("---- Done ----")
        return(refl.mat.keep)
}

##' @name gen.spec.list
##' @title List available species
##' @details {
##' Creates a list of available species tags, for reference or for batch jobs.
##' }
##' 
##' @param path Path to '.csv' spectra (default="data/SE_spectra/Reflectance/")
##' @param out Store in local memory? If TRUE, returns character vector with list. If FALSE, prints to "scripts/species_list.txt".
##' @return If out==TRUE, returns character vector of species tags.
##' @export
##' @author Alexey Shiklomanov

gen.spec.list <- function(path="data/SE_spectra/Reflectance/",
                          out=FALSE){
        flist <- list.files(path)
        flist.split <- strsplit(flist, "_")
        spec.list <- unique(sapply(flist.split, "[", 5))
        if(out) {
                return(spec.list)
        } else {
                write(spec.list, "scripts/species_list.txt",
                      ncolumns = 1, append=FALSE)
        }
}
